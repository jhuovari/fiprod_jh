## Eurostat whole economy national accounts, reshaped into the same form as the
## main table of the OECD productivity database (see data-raw/get_oecd_pdb.R).
##
## Companion to data-raw/get_eurostat_na.R, which does the same for industries.
## This script only fetches what the industry table does not already have:
## gross domestic product and population. Value added, employment and hours for
## the whole economy (`_T`) and the business sector (`BTNXL`) come from
## dat_eurostat_na_ind, and data-raw/data_main.R puts the two together.
##
## Population is not published as such, so it is backed out from GDP in millions
## of euro (nama_10_gdp) and GDP in euro per capita (nama_10_pc - the per capita
## units are in that table, not in nama_10_gdp). Eurostat rounds the per capita
## figure, which leaves a rounding error of the order of 0.01 % in the population
## level; it cancels out of any index or growth rate.
##
## Units follow Eurostat: GDP is in millions of national currency, and population
## in thousands of persons, matching the thousands Eurostat uses for employment
## and hours in get_eurostat_na.R. data_main.R checks the multipliers against the
## OECD with compare_sources().

library(tidyverse)
library(eurostat)

devtools::load_all()

options(timeout = 600)


## Classifications ------------------------------------------------------------

# Eurostat na_item -> OECD MEASURE
key_na_item <- c("B1GQ" = "GDP", "B1G" = "GVA")

# Eurostat unit -> OECD PRICE_BASE, as in get_eurostat_na.R
key_price_base <- c("CP_MNAC" = "V", "CLV20_MNAC" = "LR", "PYP_MNAC" = "Y")

# Units needed to back out population. GDP in millions of euro is in
# nama_10_gdp, GDP in euro per capita only in nama_10_pc.
key_pop_unit_gdp <- "CP_MEUR"
key_pop_unit_pc  <- "CP_EUR_HAB"

eurostat_ref_year <- 2020


## Download -------------------------------------------------------------------

dat_gdp_0 <- get_eurostat(
  "nama_10_gdp",
  time_format = "date",
  cache = FALSE,
  filters = list(na_item = names(key_na_item),
                 unit = c(names(key_price_base), key_pop_unit_gdp))
)

dat_pc_0 <- get_eurostat(
  "nama_10_pc",
  time_format = "date",
  cache = FALSE,
  filters = list(na_item = "B1GQ", unit = key_pop_unit_pc)
)


## Reshape --------------------------------------------------------------------

tidy_eurostat_raw <- function(x) {
  x |>
    rename(any_of(c(time = "TIME_PERIOD"))) |>
    select(-any_of("freq")) |>
    mutate(geo = as_factor(as.character(geo)))
}

dat_gdp_1 <- tidy_eurostat_raw(dat_gdp_0)
dat_pc_1  <- tidy_eurostat_raw(dat_pc_0)

# Gross domestic product and total gross value added, in national currency
dat_gdp_nac <-
  dat_gdp_1 |>
  filter(unit %in% names(key_price_base)) |>
  transmute(
    time,
    geo,
    measure      = unname(key_na_item[as.character(na_item)]),
    activity     = "_T",
    unit_measure = "XDC",                # millions of national currency
    price_base   = unname(key_price_base[as.character(unit)]),
    values
  )

# Population, backed out from GDP in millions of euro (nama_10_gdp) and GDP in
# euro per capita (nama_10_pc). Reported in thousands of persons, the unit
# Eurostat uses for employment, so that hours per capita comes out per person.
dat_pop <-
  dat_gdp_1 |>
  filter(na_item == "B1GQ", unit == key_pop_unit_gdp) |>
  select(time, geo, CP_MEUR = values) |>
  inner_join(
    select(filter(dat_pc_1, unit == key_pop_unit_pc), time, geo, CP_EUR_HAB = values),
    by = c("time", "geo")
  ) |>
  transmute(
    time,
    geo,
    measure      = "POP",
    activity     = "_T",
    unit_measure = "PS",                 # thousands of persons
    price_base   = "_Z",
    values       = (CP_MEUR * 1e6 / CP_EUR_HAB) / 1e3
  )

dat_eurostat_na_gdp <-
  bind_rows(dat_gdp_nac, dat_pop) |>
  mutate(
    measure      = factor(measure, levels = c("GDP", "GVA", "POP")),
    activity     = factor(activity, levels = "_T"),
    unit_measure = factor(unit_measure, levels = c("XDC", "PS")),
    price_base   = factor(price_base, levels = c("V", "LR", "Y", "_Z"))
  ) |>
  filter(!is.na(values)) |>
  arrange(geo, measure, price_base, time)

save_dat(dat_eurostat_na_gdp, overwrite = TRUE)


## Checks ---------------------------------------------------------------------

if (FALSE) {

  check_factor_levels(dat_eurostat_na_gdp, drop_vars = "geo") |> print(n = Inf)

  # The chain linked GDP has to equal the current price GDP in the base year.
  dat_eurostat_na_gdp |>
    filter(measure == "GDP", price_base %in% c("V", "LR"),
           time == paste0(eurostat_ref_year, "-01-01")) |>
    select(geo, price_base, values) |>
    pivot_wider(names_from = price_base, values_from = values) |>
    mutate(diff = LR - V) |>
    filter(abs(diff) > 1e-6) |>
    print(n = Inf)

  # Population should be smooth and of a plausible size.
  dat_eurostat_na_gdp |>
    filter(measure == "POP", geo %in% c("FI", "DE", "SE")) |>
    ggplot(aes(time, values, colour = geo)) +
    geom_line()

}
