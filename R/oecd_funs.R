
utils::globalVariables(c(
  "time",
  "values",
  "obsValue"
))

#' Clean and reshape OECD SDMX data
#'
#' This function parses dates, converts character columns to factors,
#' selects and renames specified variables, creates a `values` column
#' from `obsValues` (or `obsValue`), and drops all other columns.
#' It warns if any dropped factor variables have more than one level.
#'
#' @param df A data.frame containing OECD SDMX data.
#' @param vars A named character vector specifying variables to keep and rename,
#'   in the form \code{c(new_name = "old_name")}. The old names must exist in \code{df}.
#' @param freq Data frequency: \code{"A"} for annual data, \code{"Q"} for quarterly data.
#'   Determines the date parsing format for the \code{obsTime} column.
#'
#' @return A tibble with renamed variables from \code{vars} and a \code{values} column.
#' @examples
#' \dontrun{
#' vars <- c(geo = "LOCATION", measure = "MEASURE")
#' cleaned <- oecd_clean_data(smdx_data, vars = vars, freq = "A")
#' }
#' @export
oecd_clean_data <- function(df = NULL, sdmx_obj = NULL, drop_vars = NULL, vars = c(), freq = "A") {


  # Backward compatibility: allow passing an SDMX object via `sdmx_obj`
  if (!is.null(sdmx_obj)) {
    if (!is.null(df)) {
      warning("Both `df` and `sdmx_obj` were supplied; using `sdmx_obj`.")
    }
    df <- as.data.frame(sdmx_obj)
  }

  if (is.null(df)) {
    stop("Provide either `df` or `sdmx_obj`.")
  }

  # Validate 'vars' is named (or empty)
  if (length(vars) > 0) {
    if (is.null(names(vars)) || any(is.na(names(vars))) || any(names(vars) == "")) {
      stop("'vars' must be a named character vector in the form c(new_name = old_name).")
    }
  }

  # Decide date format based on frequency argument
  d_format <- switch(
    freq,
    "A" = "Y",
    "Q" = "y-q",
    stop("Invalid 'freq' argument. Use 'A' for annual or 'Q' for quarterly.")
  )

  # Parse dates, convert character columns to factors and rename values column.
  df2 <- df |>
    dplyr::select(-tidyselect::all_of(drop_vars)) |>
    dplyr::rename(tidyselect::any_of(c(obsTime = "TIME_PERIOD"))) |>
    pxwebtools::parse_dates(date_format = c(obsTime = d_format)) |>
    dplyr::rename(tidyselect::any_of(c(obsValue = "ObsValue"))) |>
    dplyr::mutate(
      values = as.numeric(obsValue),
      .keep = "unused"
    ) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), forcats::as_factor))

  # Validate that old names in vars exist in the data
  old_names <- unname(vars)
  new_names <- names(vars)
  missing_old <- setdiff(old_names, names(df2))
  if (length(missing_old)) {
    stop("The following old names from 'vars' were not found in the data: ",
         paste(missing_old, collapse = ", "))
  }

  # Columns that will be dropped (all except vars' old names and value_col)
  keep_raw <- c("time", old_names, "values")
  drop_cands <- setdiff(names(df2), keep_raw)

  # Check: factor columns to be dropped
  factor_drops <- drop_cands[vapply(drop_cands, function(s) is.factor(df2[[s]]), logical(1))]
  multi_level <- factor_drops[vapply(factor_drops, function(s) {
    nlevels(droplevels(df2[[s]])) > 1
  }, logical(1))]

  if (length(multi_level)) {
    warning(
      "The following factor columns to be dropped have more than 1 level: ",
      paste(multi_level, collapse = ", "),
      ". Consider keeping or handling them before dropping."
    )
  }

  # Keep only selected columns + values, rename, and drop others
  out <- df2 |>
    dplyr::select(dplyr::all_of(keep_raw)) |>
    # Rename according to vars (new = old)
    dplyr::rename(!!!stats::setNames(old_names, new_names)) |>
    # Order: selected vars first, then values
    dplyr::select(time, dplyr::all_of(new_names), values)

  out
}

#' Create an OECD Filter String
#'
#' This function takes a list of filtering arguments and constructs a filter string for an OECD API call.
#'
#' @param x A list of strings representing different filter criteria for the OECD API.
#'
#' @return A single string with the filter criteria concatenated using '+' and '.' as separators.
#'
#' @export
#'
#' @examples
#' make_oecd_filter(list("A", "sna_geo", "", "", "sna6a_transact", "", "sna_activity", "", "", "sna_measures", "", ""))
oecd_make_filter <- function(x) {
  x |>
    purrr::map(~paste(.x, collapse = "+")) |>
    paste(collapse = ".")
}
