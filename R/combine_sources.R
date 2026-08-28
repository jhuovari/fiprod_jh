#' Combine data sources country by country
#'
#' Picks each country from the first source that has it. The OECD productivity
#' database is updated more slowly than Eurostat, so the EU (and EEA) countries
#' are normally taken from Eurostat and the rest from the OECD.
#'
#' @param ... Named data frames in priority order, e.g.
#'   `combine_geo_sources(eurostat = dat_eurostat, oecd = dat_oecd)`. Only the
#'   columns shared by all sources are kept.
#' @param geos Optional named list restricting which countries are taken from
#'   which source, e.g. `list(eurostat = geo_ea)`. Sources without an entry
#'   contribute every country that no earlier source provided.
#' @param geo Name of the country column.
#' @param source_col Name of a column recording which source each row came from,
#'   or `NULL` to leave it out.
#'
#' @return A data frame with the rows of all sources, each country from one
#'   source only.
#'
#' @seealso [compare_sources()] to check that the sources are on the same scale
#'   before combining them.
#'
#' @examples
#' eurostat <- tibble::tibble(geo = c("FI", "SE"), time = 2020, values = 1:2)
#' oecd     <- tibble::tibble(geo = c("FI", "US"), time = 2020, values = 3:4)
#'
#' # FI comes from Eurostat, US from the OECD
#' combine_geo_sources(eurostat = eurostat, oecd = oecd)
#'
#' @export
combine_geo_sources <- function(..., geos = NULL, geo = "geo", source_col = "source") {
  dfs <- rlang::list2(...)
  if (!length(dfs)) stop("Give at least one data frame.")

  nms <- names(dfs)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop("All sources must be named, e.g. `combine_geo_sources(eurostat = x, oecd = y)`.")
  }
  if (anyDuplicated(nms)) {
    stop("Source names must be unique.")
  }
  if (!all(vapply(dfs, is.data.frame, logical(1)))) {
    stop("All sources must be data frames.")
  }
  no_geo <- nms[!vapply(dfs, function(d) geo %in% names(d), logical(1))]
  if (length(no_geo)) {
    stop("No `", geo, "` column in source(s): ", paste(no_geo, collapse = ", "), ".")
  }
  if (!is.null(geos)) {
    unknown <- setdiff(names(geos), nms)
    if (length(unknown)) {
      stop("`geos` names must be source names, unknown: ",
           paste(unknown, collapse = ", "), ".")
    }
  }

  all_names <- unique(unlist(lapply(dfs, names)))
  if (!is.null(source_col) && source_col %in% all_names) {
    stop("`source_col` = \"", source_col, "\" is already a column of a source.")
  }

  common <- Reduce(intersect, lapply(dfs, names))
  if (!length(common)) stop("The sources have no columns in common.")
  dropped <- setdiff(all_names, common)
  if (length(dropped)) {
    message("Dropping column(s) not shared by all sources: ",
            paste(dropped, collapse = ", "), ".")
  }

  taken <- character()
  out <- vector("list", length(dfs))

  for (i in seq_along(dfs)) {
    d <- dplyr::as_tibble(dfs[[i]])[, common, drop = FALSE]
    g <- as.character(d[[geo]])

    keep <- !g %in% taken
    if (!is.null(geos) && nms[i] %in% names(geos)) {
      keep <- keep & g %in% as.character(geos[[nms[i]]])
    }

    d <- d[keep, , drop = FALSE]
    if (!is.null(source_col)) d[[source_col]] <- nms[i]

    taken <- union(taken, unique(as.character(d[[geo]])))
    out[[i]] <- d
  }

  dplyr::bind_rows(out)
}

#' Compare two data sources on their overlap
#'
#' Reports how two sources relate on the observations they share. Use it before
#' [combine_geo_sources()] to check that the series are on the same scale (a
#' median ratio of e.g. 1000 means one source is in millions and the other in
#' thousands) and that they measure the same thing (a growth correlation well
#' below one means they do not).
#'
#' @param x,y Data frames to compare.
#' @param by Character vector of columns identifying a series.
#' @param time Name of the time column.
#' @param values Name of the value column.
#'
#' @return A data frame with one row per series: the number of shared
#'   observations `n`, the median, smallest and largest ratio of `x` to `y`, and
#'   `growth_cor`, the correlation of the yearly relative changes.
#'
#' @examples
#' x <- tibble::tibble(geo = "FI", time = 2018:2020, values = c(100, 110, 120))
#' y <- tibble::tibble(geo = "FI", time = 2018:2020, values = c(0.1, 0.11, 0.12))
#' compare_sources(x, y, by = "geo")
#'
#' @export
compare_sources <- function(x, y,
                            by = c("geo", "activity", "measure", "price_base"),
                            time = "time",
                            values = "values") {
  by <- intersect(by, intersect(names(x), names(y)))
  need <- c(time, values)
  for (nm in c("x", "y")) {
    d <- get(nm)
    miss <- setdiff(need, names(d))
    if (length(miss)) {
      stop("Missing column(s) in `", nm, "`: ", paste(miss, collapse = ", "), ".")
    }
  }

  prep <- function(d, suffix) {
    d <- dplyr::as_tibble(d)[, c(by, time, values), drop = FALSE]
    d[by] <- lapply(d[by], as.character)
    names(d)[names(d) == values] <- suffix
    d
  }

  joined <- dplyr::inner_join(prep(x, ".x"), prep(y, ".y"), by = c(by, time))
  joined <- joined[stats::complete.cases(joined[, c(".x", ".y")]), , drop = FALSE]

  joined |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::arrange(.data[[time]], .by_group = TRUE) |>
    dplyr::summarise(
      n            = dplyr::n(),
      ratio_median = stats::median(.data$.x / .data$.y, na.rm = TRUE),
      ratio_min    = suppressWarnings(min(.data$.x / .data$.y, na.rm = TRUE)),
      ratio_max    = suppressWarnings(max(.data$.x / .data$.y, na.rm = TRUE)),
      growth_cor   = .growth_cor(.data$.x, .data$.y),
      .groups = "drop"
    )
}

#' Correlation of yearly relative changes
#'
#' @param x,y Numeric vectors ordered by time.
#' @keywords internal
.growth_cor <- function(x, y) {
  if (length(x) < 3L) return(NA_real_)
  gx <- x[-1] / x[-length(x)]
  gy <- y[-1] / y[-length(y)]
  ok <- is.finite(gx) & is.finite(gy)
  if (sum(ok) < 2L) return(NA_real_)
  suppressWarnings(stats::cor(gx[ok], gy[ok]))
}
