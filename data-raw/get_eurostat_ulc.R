## Eurostat national accounts for the price competitiveness indicators.
##
## Brings over the core of the old ficomp package
## (https://github.com/pttry/ficomp), which the productivity board's
## competitiveness chapter was built on. Only what the board figures need is
## fetched: gross domestic product, compensation of employees, exports and
## imports for the terms of trade adjustment, and the labour input.
##
## Everything is the whole economy, so the industry tables are not involved.
## Employees and employed persons are taken at NACE TOTAL from nama_10_a10_e,
## which keeps this script independent of get_eurostat_na.R.
##
## Units follow Eurostat: money in millions of national currency (or euro),
## labour input in thousands. The indicators are all ratios or indices, so the
## multipliers cancel; data_main.R does not scale anything here.

library(tidyverse)
library(eurostat)

devtools::load_all()

options(timeout = 600)


## What is needed -------------------------------------------------------------

# nama_10_gdp transactions
#   B1GQ  gross domestic product
#   B1G   gross value added
#   D1    compensation of employees
#   P6    exports of goods and services      (P61 goods, P62 services)
#   P7    imports of goods and services
key_na_item <- c("B1GQ", "B1G", "D1", "P6", "P61", "P62", "P7")

# Current prices in national currency and in euro, and the chain linked volume.
# D1 in euro divided by D1 in national currency is the exchange rate, which is
# how the common currency measure picks up currency movements.
key_unit_gdp <- c("CP_MNAC", "CP_MEUR", "CLV20_MNAC")

# nama_10_a10_e, NACE TOTAL
#   EMP_DC  total employment, domestic concept   -> output per employed person
#   SAL_DC  employees, domestic concept          -> compensation per employee
key_na_item_e <- c("EMP_DC", "SAL_DC")
key_unit_e    <- c("THS_PER", "THS_HW")

ulc_ref_year <- 2020


## Download -------------------------------------------------------------------

dat_gdp_ulc_0 <- get_eurostat(
  "nama_10_gdp",
  time_format = "date",
  cache = FALSE,
  filters = list(na_item = key_na_item, unit = key_unit_gdp, geo = geos_comp_es)
)

dat_emp_ulc_0 <- get_eurostat(
  "nama_10_a10_e",
  time_format = "date",
  cache = FALSE,
  filters = list(na_item = key_na_item_e, unit = key_unit_e,
                 nace_r2 = "TOTAL", geo = geos_comp_es)
)


## Reshape --------------------------------------------------------------------

# One column per transaction and unit, as ficomp had them: B1GQ__CLV20_MNAC and
# so on. Wide is the right shape here, because every indicator combines several
# transactions.
dat_eurostat_ulc <-
  bind_rows(
    dat_gdp_ulc_0 |>
      rename(any_of(c(time = "TIME_PERIOD"))) |>
      select(geo, time, na_item, unit, values),
    dat_emp_ulc_0 |>
      rename(any_of(c(time = "TIME_PERIOD"))) |>
      select(geo, time, na_item, unit, values)
  ) |>
  mutate(geo = as.character(geo)) |>
  unite("vars", na_item, unit, sep = "__") |>
  pivot_wider(names_from = vars, values_from = values) |>
  filter(geo %in% geos_comp_es) |>
  mutate(geo = factor(geo, levels = geos_comp_es)) |>
  arrange(geo, time)

save_dat(dat_eurostat_ulc, overwrite = TRUE)


## Checks ---------------------------------------------------------------------

if (FALSE) {

  # Which countries and years are missing what. The competitiveness peers
  # (geos_comp) all have to be complete, or the weighted relatives turn into NA.
  dat_eurostat_ulc |>
    filter(geo %in% geos_comp) |>
    summarise(across(where(is.numeric), ~ sum(is.na(.x))), .by = geo) |>
    print(n = Inf)

  # The chain linked volume equals the current price value in the base year.
  dat_eurostat_ulc |>
    filter(lubridate::year(time) == ulc_ref_year) |>
    transmute(geo, diff = B1GQ__CLV20_MNAC - B1GQ__CP_MNAC) |>
    filter(abs(diff) > 1e-6) |>
    print(n = Inf)

  # The exchange rate implied by D1 should be flat at 1 for the euro countries.
  dat_eurostat_ulc |>
    transmute(geo, time, exch = D1__CP_MNAC / D1__CP_MEUR) |>
    filter(geo %in% c("FI", "DE", "SE", "DK", "NO")) |>
    ggplot(aes(time, exch, colour = geo)) +
    geom_line() +
    scale_y_log10()

}
