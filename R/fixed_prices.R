utils::globalVariables(c(
  ".activity",
  ".price",
  ".values",
  ".time",
  ".n"
))

#' Year of a time vector
#'
#' Accepts the `Date` columns used throughout the package as well as plain
#' numeric, character or factor years.
#'
#' @param x A vector of dates or years.
#' @return An integer vector of years.
#' @keywords internal
.as_year <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(as.integer(format(x, "%Y")))
  }
  if (is.factor(x) || is.character(x)) {
    return(as.integer(substr(as.character(x), 1, 4)))
  }
  as.integer(x)
}

#' Convert between fixed and previous year's price series
#'
#' Chain linked volume series cannot be added together over industries (or any
#' other breakdown), because every series carries its own chain of price
#' structures. Series in previous year's prices (PYP) can be added, since within
#' a year all of them are valued at the same, previous year's, price level.
#' Aggregating a volume series therefore takes three steps: convert the
#' components to previous year's prices, add them up, and chain the sum back to
#' a fixed price series.
#'
#' `prev_year_prices()` does the first step and `fixed_prices()` the last one:
#'
#' \deqn{pyp_t = cp_{t-1} \times fp_t / fp_{t-1}}
#' \deqn{fp_t = fp_{t-1} \times pyp_t / cp_{t-1}}
#'
#' `fixed_prices()` chains outwards from `ref_year` in both directions, so the
#' result equals `cp` in the reference year. The chain stops at a break (a
#' missing `cp` or `pyp`, or a gap in `time`) and all later (earlier) years are
#' `NA`, because a volume series cannot be linked across a break.
#'
#' The functions are yearly-data equivalents of `statfitools::pp()` and
#' `statfitools::fp()`.
#'
#' @param cp A numeric vector of current price values.
#' @param fp A numeric vector of fixed price (chain linked volume) values.
#' @param pyp A numeric vector of previous year's price values.
#' @param time A vector of dates or years. One observation per year is required,
#'   but the years need not be sorted or consecutive.
#' @param ref_year Numeric reference (base) year of the returned fixed price
#'   series. Must be present in `time`.
#'
#' @return A numeric vector as long as `time`.
#'
#' @examples
#' cp   <- c(100, 120, 150)
#' fp20 <- c(90, 110, 150)
#' time <- 2018:2020
#'
#' pyp <- prev_year_prices(cp, fp20, time)
#' pyp
#'
#' # Chaining back reproduces the original volume series
#' fixed_prices(cp, pyp, time, ref_year = 2020)
#'
#' @importFrom rlang .data :=
#' @export
prev_year_prices <- function(cp, fp, time) {
  year <- .as_year(time)
  if (length(cp) != length(fp) || length(cp) != length(year)) {
    stop("`cp`, `fp` and `time` must have the same length.")
  }
  dup <- unique(year[duplicated(year)])
  if (length(dup)) {
    stop("`time` must have at most one observation per year, duplicated: ",
         paste(dup, collapse = ", "), ".")
  }
  prev <- match(year - 1L, year)
  as.numeric(cp[prev] * fp / fp[prev])
}

#' @rdname prev_year_prices
#' @export
fixed_prices <- function(cp, pyp, time, ref_year = 2020) {
  year <- .as_year(time)
  if (length(cp) != length(pyp) || length(cp) != length(year)) {
    stop("`cp`, `pyp` and `time` must have the same length.")
  }
  dup <- unique(year[duplicated(year)])
  if (length(dup)) {
    stop("`time` must have at most one observation per year, duplicated: ",
         paste(dup, collapse = ", "), ".")
  }
  ref_year <- .as_year(ref_year)
  if (length(ref_year) != 1L || is.na(ref_year)) {
    stop("`ref_year` must be a single year.")
  }

  ord <- order(year)
  y   <- year[ord]
  cp  <- as.numeric(cp)[ord]
  pyp <- as.numeric(pyp)[ord]
  n   <- length(y)

  i0 <- match(ref_year, y)
  if (is.na(i0)) {
    stop("`ref_year` ", ref_year, " is not present in `time`.")
  }

  out <- rep(NA_real_, n)
  base <- cp[i0]

  if (!is.na(base)) {
    # Volume change from t-1 to t. NA whenever the previous year is missing,
    # which breaks the chain exactly where it should be broken.
    prev <- match(y - 1L, y)
    chg <- pyp / cp[prev]
    chg[!is.finite(chg)] <- NA_real_

    out[i0] <- base
    if (i0 < n) {
      out[(i0 + 1L):n] <- base * cumprod(chg[(i0 + 1L):n])
    }
    if (i0 > 1L) {
      back <- cumprod(chg[seq.int(i0, 2L)])
      out[i0 - seq_along(back)] <- base / back
    }
  }

  res <- rep(NA_real_, n)
  res[ord] <- out
  res
}
