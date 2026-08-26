#' Chain a fixed price series, returning NA instead of failing
#'
#' @inheritParams fixed_prices
#' @param label A label used in the warning message.
#' @keywords internal
.fixed_prices_safe <- function(cp, pyp, time, ref_year, label = NULL) {
  tryCatch(
    fixed_prices(cp, pyp, time, ref_year),
    error = function(e) {
      warning("Could not chain ", label, ": ", conditionMessage(e),
              call. = FALSE)
      rep(NA_real_, length(time))
    }
  )
}

#' Aggregate national accounts industries
#'
#' Builds industry aggregates (such as the OECD business sector `BTNXL`, i.e.
#' NACE B to N excluding L) from a long data frame of national accounts series.
#'
#' Current price and previous year's price series are simply added up. Chain
#' linked volumes are not additive, so they are converted to previous year's
#' prices with [prev_year_prices()], added up, and chained back with
#' [fixed_prices()]. When the data already contains previous year's price series
#' (Eurostat publishes them, OECD does not) those are used and only the missing
#' ones are derived from the current and fixed price series.
#'
#' An aggregate is `NA` in every year in which any of its components is missing,
#' so that a partial sum is never mistaken for the aggregate.
#'
#' @param df A long data frame with an activity column, a price base column, a
#'   time column and a numeric value column. All remaining columns (`geo`,
#'   `measure`, `unit_measure`, ...) identify a series and are kept as they are.
#'   Columns that encode the price base themselves (such as the `var_id` of the
#'   OECD tables) must be dropped first; the function stops if it finds one.
#' @param key A named list mapping each new activity code to its components,
#'   e.g. `list(BTNXL = c("BTE", "F", "GTNXL"))`. Components must not overlap
#'   within one aggregate.
#' @param ref_year Reference (base) year of the chained fixed price series.
#' @param cp,fp,pyp Values of the price base column identifying current price,
#'   fixed price (chain linked volume) and previous year's price series.
#'   Previous year's price series are derived from `cp` and `fp` when absent.
#' @param additive Values of the price base column that may be added up as they
#'   are, such as `"_Z"` for employment and hours. Series that are neither
#'   additive nor one of `cp`, `fp`, `pyp` (chain linked indices, ratios such as
#'   value added per hour) cannot be aggregated and are dropped.
#' @param activity,price_base,values,time Column names in `df`.
#' @param append If `TRUE` (default) the aggregates are added to `df`, replacing
#'   any existing rows for the same activity codes. If `FALSE` only the
#'   aggregates are returned.
#'
#' @return A data frame with the same columns as `df`.
#'
#' @seealso [prev_year_prices()], [combine_geo_sources()]
#'
#' @examples
#' dat <- tibble::tibble(
#'   geo        = "FI",
#'   measure    = "GVA",
#'   activity   = rep(c("F", "J"), each = 6),
#'   price_base = rep(rep(c("V", "LR"), each = 3), 2),
#'   time       = rep(2018:2020, 4),
#'   values     = c(100, 110, 120,  95, 105, 120,   # construction
#'                   50,  52,  60,  48,  50,  60)   # information
#' )
#'
#' aggregate_activities(dat, list(FJ = c("F", "J")), ref_year = 2020)
#'
#' @export
aggregate_activities <- function(df,
                                 key,
                                 ref_year = 2020,
                                 cp = "V",
                                 fp = "LR",
                                 pyp = "Y",
                                 additive = "_Z",
                                 activity = "activity",
                                 price_base = "price_base",
                                 values = "values",
                                 time = "time",
                                 append = TRUE) {

  ## ---- input checks ------------------------------------------------------
  req <- c(activity, price_base, values, time)
  missing_cols <- setdiff(req, names(df))
  if (length(missing_cols)) {
    stop("Missing column(s) in `df`: ", paste(missing_cols, collapse = ", "), ".")
  }
  if (anyDuplicated(req)) {
    stop("`activity`, `price_base`, `values` and `time` must name different columns.")
  }
  if (!is.list(key) || !length(key) || is.null(names(key)) ||
      any(!nzchar(names(key)))) {
    stop("`key` must be a named list, e.g. `list(BTNXL = c('BTE', 'F', 'GTNXL'))`.")
  }
  if (!is.numeric(df[[values]])) {
    stop("Column `", values, "` must be numeric.")
  }

  internal <- c(".activity", ".price", ".values", ".time")
  clash <- intersect(internal, setdiff(names(df), req))
  if (length(clash)) {
    stop("`df` must not contain column(s) named ", paste(clash, collapse = ", "),
         "; they are used internally.")
  }

  comps <- unique(unlist(key, use.names = FALSE))
  unknown <- setdiff(comps, unique(as.character(df[[activity]])))
  if (length(unknown)) {
    stop("Component activit(ies) not found in `df$", activity, "`: ",
         paste(unknown, collapse = ", "), ".")
  }
  overlapping <- names(key)[vapply(key, anyDuplicated, integer(1)) > 0]
  if (length(overlapping)) {
    stop("Duplicated components in `key` element(s): ",
         paste(overlapping, collapse = ", "), ".")
  }

  ## ---- switch to internal column names -----------------------------------
  act_levels   <- if (is.factor(df[[activity]]))   levels(df[[activity]])   else NULL
  price_levels <- if (is.factor(df[[price_base]])) levels(df[[price_base]]) else NULL
  out_names <- names(df)

  d <- dplyr::as_tibble(df)
  names(d)[match(req, names(d))] <- internal
  d$.activity <- as.character(d$.activity)
  d$.price    <- as.character(d$.price)
  d$.values   <- as.numeric(d$.values)

  id_cols <- setdiff(names(d), internal)

  ## Columns that encode the price base (e.g. `var_id`) would split the
  ## current and fixed price rows into separate series.
  if (length(id_cols) && dplyr::n_distinct(d$.price) > 1L) {
    prices_per_level <- vapply(id_cols, function(nm) {
      max(tapply(d$.price, as.character(d[[nm]]), dplyr::n_distinct), na.rm = TRUE)
    }, numeric(1))
    bad <- id_cols[prices_per_level == 1]
    if (length(bad)) {
      stop("Column(s) ", paste(bad, collapse = ", "),
           " identify the price base and would split the series. Drop them first.")
    }
  }

  handled <- c(cp, fp, pyp, additive)
  ignored <- setdiff(unique(d$.price[d$.activity %in% comps]), handled)
  if (length(ignored)) {
    message("Not aggregated (neither additive nor a price base): ",
            paste(ignored, collapse = ", "), ".")
  }

  parts <- d[d$.activity %in% comps & d$.price %in% handled, , drop = FALSE]

  ## ---- aggregate ---------------------------------------------------------
  agg <- lapply(names(key), function(nm) {
    comps_i <- key[[nm]]
    k <- length(comps_i)
    x <- parts[parts$.activity %in% comps_i, , drop = FALSE]
    if (!nrow(x)) return(NULL)

    pieces <- list()

    vol_prices <- intersect(c(cp, fp, pyp), unique(x$.price))
    if (length(vol_prices)) {
      if (fp %in% vol_prices && !cp %in% vol_prices) {
        warning("No `", cp, "` series for ", nm,
                "; fixed price series cannot be aggregated.", call. = FALSE)
      }
      w <- tidyr::pivot_wider(x[x$.price %in% vol_prices, , drop = FALSE],
                              names_from = ".price", values_from = ".values")
      for (p in c(cp, fp, pyp)) {
        if (!p %in% names(w)) w[[p]] <- NA_real_
      }

      w <- w |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, ".activity")))) |>
        dplyr::mutate(
          !!pyp := dplyr::coalesce(
            .data[[pyp]],
            prev_year_prices(.data[[cp]], .data[[fp]], .data$.time))) |>
        dplyr::ungroup()

      s <- w |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, ".time")))) |>
        dplyr::summarise(
          .n = dplyr::n_distinct(.activity),
          dplyr::across(dplyr::all_of(c(cp, pyp)), sum),
          .groups = "drop") |>
        dplyr::mutate(dplyr::across(dplyr::all_of(c(cp, pyp)),
                                    ~ dplyr::if_else(.data$.n == k, .x, NA_real_))) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
        dplyr::mutate(
          !!fp := .fixed_prices_safe(
            .data[[cp]], .data[[pyp]], .data$.time, ref_year,
            label = paste0(nm, " (", paste(unlist(dplyr::cur_group()),
                                           collapse = ", "), ")"))) |>
        dplyr::ungroup()

      pieces$volume <- s |>
        dplyr::select(-".n") |>
        tidyr::pivot_longer(dplyr::all_of(vol_prices),
                            names_to = ".price", values_to = ".values")
    }

    add_prices <- intersect(additive, unique(x$.price))
    if (length(add_prices)) {
      pieces$additive <- x[x$.price %in% add_prices, , drop = FALSE] |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, ".price", ".time")))) |>
        dplyr::summarise(.n = dplyr::n_distinct(.activity),
                         .values = sum(.data$.values),
                         .groups = "drop") |>
        dplyr::mutate(.values = dplyr::if_else(.data$.n == k, .data$.values, NA_real_)) |>
        dplyr::select(-".n")
    }

    res <- dplyr::bind_rows(pieces)
    res$.activity <- nm
    res
  })

  agg <- dplyr::bind_rows(agg)

  ## ---- back to the original shape ----------------------------------------
  names(agg)[match(internal, names(agg))] <- req

  if (isTRUE(append)) {
    kept <- dplyr::as_tibble(df)[!as.character(df[[activity]]) %in% names(key), , drop = FALSE]
    agg <- dplyr::bind_rows(kept, agg)
  }

  if (!is.null(act_levels)) {
    agg[[activity]] <- factor(as.character(agg[[activity]]),
                              levels = union(act_levels, names(key)))
  }
  if (!is.null(price_levels)) {
    agg[[price_base]] <- factor(as.character(agg[[price_base]]), levels = price_levels)
  }

  agg[, out_names, drop = FALSE]
}
