## Documentation for the data shipped with the package. The objects themselves
## are built in data-raw/: the country groups in data_main.R, the exchange rates
## in get_exch.R, the trade weights in get_ecfin_weights.R and the country names
## in naming.R.

#' Euro area countries
#'
#' The twenty countries of the euro area, as Eurostat two letter codes (`EL` for
#' Greece, not `GR`).
#'
#' @format A character vector of length 20.
#' @source data-raw/data_main.R
"geo_ea"

#' Countries of the OECD productivity database query
#'
#' The countries fetched from the OECD productivity database, as ISO three
#' letter codes, with `EA20` for the euro area aggregate. The three letter codes
#' are what the OECD API wants; they are turned into Eurostat codes when the
#' data is read.
#'
#' @format A character vector of length 16.
#' @source data-raw/data_main.R, used by data-raw/get_oecd_pdb.R
"geos_oecd"

#' Countries taken from Eurostat when the sources are combined
#'
#' The OECD productivity database lags Eurostat by several months, so these
#' countries are taken from Eurostat and the rest from the OECD. Everything
#' outside this vector — the US, Japan and the United Kingdom, which Eurostat no
#' longer updates — comes from the OECD.
#'
#' @format A character vector of length 24: `EA20`, the euro area countries,
#'   Sweden, Denmark and Norway.
#' @seealso [combine_geo_sources()]
#' @source data-raw/data_main.R
"geos_eurostat"

#' Peer group of the price competitiveness indicators
#'
#' The seventeen countries Finland's relative unit labour costs are weighted
#' against, as in the old ficomp package. `geos_comp_es` are the fifteen covered
#' by Eurostat's national accounts, Switzerland included through EFTA;
#' `geos_comp_oecd` are the two that are not and come from the OECD
#' productivity database instead. That database carries the unit labour cost and
#' its parts but no exports or imports, so the terms of trade adjusted measures
#' exist for the fifteen only.
#'
#' @format Character vectors of length 17, 15 and 2.
#' @seealso [weight_index2()], [ind_ulc()]
#' @source data-raw/data_main.R
"geos_comp"

#' @rdname geos_comp
"geos_comp_es"

#' @rdname geos_comp
"geos_comp_oecd"

#' Index base year of the competitiveness indicators
#'
#' The year the unit labour cost indices are set to 100 before they are made
#' relative to the peer group, as in ficomp.
#'
#' @format A single number.
#' @seealso [ind_ulc()], [rebase_index()]
#' @source data-raw/data_main.R
"comp_base_year"

#' Country names in Finnish and English
#'
#' Names for the countries the reports use, for labelling figures and tables.
#'
#' @format A list of two named character vectors, `fi` and `en`, each named by
#'   Eurostat country code.
#' @seealso [geo_names()]
#' @source data-raw/naming.R
"geo_names_list"

#' Annual exchange rates against the euro
#'
#' National currency per euro, yearly averages from Eurostat, from 1971 onwards.
#' The euro area countries carry a rate of 1 for `EUR`, so that a conversion
#' works the same way for every country.
#'
#' @format A tibble with four columns:
#' \describe{
#'   \item{time}{Year, as a date on the first of January.}
#'   \item{currency}{Currency code, e.g. `SEK`.}
#'   \item{values}{Units of the currency per euro.}
#'   \item{geo}{Country the currency belongs to, as a Eurostat code.}
#' }
#' @seealso [convert_currency()]
#' @source data-raw/get_exch.R, Eurostat
"exh_eur_a"

#' Trade weights of the European Commission
#'
#' Double export weights from DG ECFIN's price and cost competitiveness data,
#' used to weight a country's peers into one relative figure. The number in the
#' name is the size of the peer group the weights are normalised over: the euro
#' area 19 and 20, the EU 27, and the industrial country groups of 37 and 42.
#' The weights of one base country in one year sum to one.
#'
#' @format A data frame with four columns:
#' \describe{
#'   \item{geo_base}{The country whose peers are being weighted.}
#'   \item{time}{Year.}
#'   \item{geo}{The peer country the weight belongs to.}
#'   \item{weight}{Share of the peer group, summing to one over `geo` within
#'     each `geo_base` and `time`. A country's weight on itself is zero.}
#' }
#' @seealso [weight_index()], [weight_index2()], [read_ecfin_weights()]
#' @source data-raw/get_ecfin_weights.R,
#'   <https://economy-finance.ec.europa.eu/economic-research-and-databases/economic-databases/price-and-cost-competitiveness/price-and-cost-competitiveness-data-section_en>
"weights_ecfin19"

#' @rdname weights_ecfin19
"weights_ecfin20"

#' @rdname weights_ecfin19
"weights_ecfin27"

#' @rdname weights_ecfin19
"weights_ecfin37"

#' @rdname weights_ecfin19
"weights_ecfin42"
