#' Extend a series forward with growth rates from another source
#'
#' Annual national accounts arrive late for some countries, while a quarterly or
#' monthly indicator of the same thing is already out. This carries the level of
#' the annual series forward using the growth of the timelier one, so a chart
#' does not have to stop a year or two short.
#'
#' Only the tail is filled: values are written after the last observation of `x`
#' and nothing before or between is touched. The result is an extrapolation, not
#' a measurement, and is worth marking as such wherever it is shown.
#'
#' @param x A numeric vector of levels or index values, with the missing tail as
#'   `NA`.
#' @param change A numeric vector of relative changes aligned to `x`, e.g.
#'   `0.03` for three per cent. Only the values after the last observation of
#'   `x` are used.
#' @param time A vector of dates or years used to order the series. The result
#'   is returned in the order it was given in.
#'
#' @return A numeric vector as long as `x`. `NA` where `x` was missing and
#'   `change` did not reach.
#'
#' @seealso [ind_ulc()], [rebase_index()]
#'
#' @examples
#' level  <- c(100, 105, 110, NA, NA)
#' growth <- c(NA, 0.05, 0.048, 0.05, 0.04)
#' time   <- 2020:2024
#'
#' extend_with_change(level, growth, time)
#'
#' # 110 * 1.05 and then * 1.04
#' @export
extend_with_change <- function(x, change, time) {
  if (length(x) != length(change) || length(x) != length(time)) {
    stop("`x`, `change` and `time` must have the same length.")
  }

  ord <- order(.as_year(time))
  xs <- as.numeric(x)[ord]
  ds <- as.numeric(change)[ord]

  observed <- which(!is.na(xs))
  if (length(observed)) {
    last <- max(observed)
    if (last < length(xs)) {
      tail_i <- seq.int(last + 1L, length(xs))
      # cumprod stops at the first missing growth rate, which is what should
      # happen: the chain cannot be carried across a gap
      xs[tail_i] <- xs[last] * cumprod(1 + ds[tail_i])
    }
  }

  out <- rep(NA_real_, length(xs))
  out[ord] <- xs
  out
}
