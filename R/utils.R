#' Tabulate factor level combinations (by time) against `values`
#'
#' Creates a complete cross-tabulation over all factor variables (excluding those
#' in \code{drop_vars}) and all unique time points, and reports the number of rows
#' and whether any non-missing numeric values exist in the \code{values} column
#' for each combination.
#'
#' @param df A \code{data.frame} containing:
#'   \itemize{
#'     \item one column named \code{time} of class \code{Date}, and
#'     \item one numeric column named \code{values}, and
#'     \item one or more factor columns (dimensions).
#'   }
#' @param drop_vars Character vector of factor column names to exclude from the
#'   cross-tabulation. Defaults to none.
#'
#' @return A \code{data.frame} with one row per combination of factor levels and
#'   \code{time}, plus \code{n} and \code{has_numeric} columns. Factor columns in
#'   the output retain the original levels and ordering.
#'
#' @examples
#' df <- data.frame(
#'   country = factor(c("FI","FI","SE","SE","NO")),
#'   sector  = factor(c("A","B","A","B","A")),
#'   time    = as.Date(c("2020-01-01","2020-01-01","2020-01-01","2021-01-01","2021-01-01")),
#'   values  = c(1, NA, NA, 3, NA)
#' )
#' check_factor_levels(df)
#'
#' # Excluding a factor from the grid:
#' check_factor_levels(df, drop_vars = "sector")
#' @export
check_factor_levels <- function(df, drop_vars = character()) {
  # Basic column checks
  if (!"values" %in% names(df)) {
    stop("The data frame must contain a 'values' column.")
  }
  if (!is.numeric(df$values)) {
    stop("'values' column must be numeric.")
  }


  # Select factor variables, excluding drop_vars
  factor_vars0 <- names(df)[vapply(df, is.factor, logical(1))]
  factor_vars <- setdiff(factor_vars0, drop_vars)

  if (length(factor_vars) == 0) {
    warning("No factor variables to tabulate after applying drop_vars.")

  }

  # Build a named list of factor levels: list(geo = levels(df$geo), nace = levels(df$nace), ...)
  lvls <- rlang::set_names(lapply(df[factor_vars0], levels), factor_vars0)

  df_full <- tidyr::complete(
    df,
    tidyr::crossing(!!!lvls)     # all factor levels (full grid for the factors)

  )


  res <- df_full |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(factor_vars)))) |>
    dplyr::summarise(
      n = dplyr::n(),
      has_numeric = any(!is.na(.data$values)),
      .groups = "drop"
    )
 res
}
