#' Unit labour cost index
#'
#' Nominal unit labour costs are labour cost per unit of output: the
#' compensation of employees divided by the volume of output. Both sides can be
#' put on a per head or per hour basis by giving the labour inputs.
#'
#' The entrepreneur adjusted measure that the productivity board reports uses
#' compensation per employee (`input1 = employees`) against output per employed
#' person (`input2 = employed`). It scales the cost of employees up to all of
#' the labour used, which matters in countries where the self-employed are a
#' large share of employment.
#'
#' Dividing the compensation in euro rather than in national currency gives the
#' common currency measure, which moves with the exchange rate as well.
#'
#' @param cost Compensation of employees, current prices.
#' @param output Value added or GDP, chain linked volume.
#' @param input1 Labour input of the cost, e.g. employees. Defaults to 1, which
#'   leaves the cost as a total.
#' @param input2 Labour input of the output, e.g. employed persons. Defaults to
#'   1.
#' @param time A vector of dates or years.
#' @param baseyear Year or years the index is set to 100 in.
#'
#' @return A numeric vector, an index with `baseyear` at 100.
#'
#' @seealso [gdp_trading_gain()] for the terms of trade adjusted output,
#'   [rebase_index()], [weight_index2()] for the weighting against peers.
#'
#' @examples
#' cost   <- c(100, 104, 110)
#' output <- c(200, 205, 208)
#' time   <- 2018:2020
#'
#' ind_ulc(cost, output, time = time, baseyear = 2020)
#'
#' # per employee against output per employed person
#' ind_ulc(cost, output, input1 = c(50, 50, 51), input2 = c(60, 60, 61),
#'         time = time, baseyear = 2020)
#'
#' @export
ind_ulc <- function(cost, output, input1 = 1, input2 = 1, time, baseyear) {
  rebase_index((cost / input1) / (output / input2), time, baseyear)
}

#' Terms of trade adjusted volume of GDP
#'
#' Real GDP measures what a country produces, not what it can buy with it. When
#' export prices rise relative to import prices the same production buys more
#' imports, and that trading gain does not show up in the volume of GDP.
#'
#' The adjusted measure, also called command basis GDP, replaces the volume of
#' exports with the volume of imports those exports could pay for:
#'
#' \deqn{gdp^{adj} = gdp - exports + \frac{exports_{cp}}{p^{imports}}}
#'
#' where the import deflator is \eqn{p^{imports} = imports_{cp} / imports}. Used
#' as the output of [ind_ulc()] it gives the terms of trade adjusted unit labour
#' cost, which is the measure that says whether a country's cost level is
#' sustainable given the prices it actually gets for its exports.
#'
#' @param gdp,exports,imports Chain linked volumes.
#' @param exports_cp,imports_cp The same exports and imports at current prices.
#'
#' @return A numeric vector in the units of `gdp`.
#'
#' @seealso [ind_ulc()]
#'
#' @examples
#' # Export prices up 10 % against import prices: the adjusted volume is higher
#' gdp_trading_gain(gdp = 100, exports = 40, exports_cp = 44,
#'                  imports = 30, imports_cp = 30)
#'
#' @export
gdp_trading_gain <- function(gdp, exports, exports_cp, imports, imports_cp) {
  import_deflator <- imports_cp / imports
  gdp - exports + exports_cp / import_deflator
}
