## Eurostat national accounts by industry, reshaped into the same form as the
## OECD productivity database (see data-raw/get_oecd_pdb.R).
##
## The OECD productivity database is updated clearly later than Eurostat, so the
## same series are taken from Eurostat for the EU/EEA countries and combined
## with the OECD data for the rest in data-raw/data_main.R.
##
## Industries are the NACE Rev. 2 A*10 aggregates. OECD publishes the business
## sector BTNXL (B to N excluding L) and its service part GTNXL (G to N
## excluding L), Eurostat does not, so they are built here with
## aggregate_activities(). Chain linked volumes are not additive: they are
## converted to previous year's prices (Eurostat publishes PYP_MNAC directly,
## which is used when available), added up and chained back to 2020 prices.
##
## Units follow Eurostat, not OECD: gross value added is in millions of national
## currency and employment in thousands of persons and thousands of hours. The
## OECD series may use another multiplier - data_main.R checks this with
## compare_sources() on the countries both sources cover.

library(tidyverse)
library(eurostat)

devtools::load_all()

options(timeout = 600)


## Classifications ------------------------------------------------------------

# NACE Rev. 2 (A*10) -> OECD ACTIVITY. C is a part of B-E and is kept only as an
# extra series, it is never one of the components summed below.
key_nace_activity <- c(
  "TOTAL" = "_T",    # total gross value added
  "A"     = "A",     # agriculture, forestry and fishing
  "B-E"   = "BTE",   # industry (except construction)
  "C"     = "C",     # manufacturing
  "F"     = "F",     # construction
  "G-I"   = "GTI",   # trade, transport, accommodation and food services
  "J"     = "J",     # information and communication
  "K"     = "K",     # financial and insurance activities
  "L"     = "L",     # real estate activities
  "M_N"   = "M_N",   # professional, scientific, technical, admin and support
  "O-Q"   = "OTQ",   # public administration, education, health
  "R-U"   = "RTU"    # arts, entertainment and other services
)

# Aggregates OECD publishes but Eurostat does not.
key_activity_agg <- list(
  GTNXL = c("GTI", "J", "K", "M_N"),
  BTNXL = c("BTE", "F", "GTI", "J", "K", "M_N")
)

# Eurostat unit -> OECD PRICE_BASE
key_price_base <- c("CP_MNAC" = "V", "CLV20_MNAC" = "LR", "PYP_MNAC" = "Y")

# Base year of CLV20_MNAC and of the OECD LR series
eurostat_ref_year <- 2020

activity_levels <- c(unname(key_nace_activity), names(key_activity_agg))


## Download -------------------------------------------------------------------

dat_a10_0 <- get_eurostat(
  "nama_10_a10",
  time_format = "date",
  cache = FALSE,
  filters = list(na_item = "B1G", unit = names(key_price_base))
)

dat_a10_e_0 <- get_eurostat(
  "nama_10_a10_e",
  time_format = "date",
  cache = FALSE,
  filters = list(na_item = "EMP_DC", unit = c("THS_PER", "THS_HW"))
)


## Reshape --------------------------------------------------------------------

# Column names differ between eurostat package versions.
tidy_eurostat_raw <- function(x) {
  x |>
    rename(any_of(c(time = "TIME_PERIOD"))) |>
    select(-any_of(c("freq", "na_item"))) |>
    filter(nace_r2 %in% names(key_nace_activity)) |>
    mutate(
      geo = as_factor(as.character(geo)),
      activity = factor(unname(key_nace_activity[as.character(nace_r2)]),
                        levels = activity_levels)
    ) |>
    select(-nace_r2)
}

# Gross value added, current / previous year's / 2020 prices
dat_gva <-
  dat_a10_0 |>
  tidy_eurostat_raw() |>
  transmute(
    time,
    geo,
    measure      = factor("GVA"),
    activity,
    unit_measure = factor("XDC"),        # millions of national currency
    price_base   = factor(unname(key_price_base[as.character(unit)]),
                          levels = unname(key_price_base)),
    values
  )

# Employment, domestic concept: persons and hours worked
dat_emp <-
  dat_a10_e_0 |>
  tidy_eurostat_raw() |>
  transmute(
    time,
    geo,
    measure      = factor(if_else(unit == "THS_HW", "HRS", "EMP"),
                          levels = c("EMP", "HRS")),
    activity,
    unit_measure = factor(if_else(unit == "THS_HW", "H", "PS"),
                          levels = c("PS", "H")),   # thousands
    price_base   = factor("_Z"),
    values
  )

dat_eurostat_na_ind_0 <-
  bind_rows(dat_gva, dat_emp) |>
  mutate(
    measure      = fct_relevel(measure, "GVA", "EMP", "HRS"),
    price_base   = fct_relevel(price_base, "V", "LR", "Y", "_Z"),
    unit_measure = fct_relevel(unit_measure, "XDC", "PS", "H")
  ) |>
  arrange(geo, measure, activity, price_base, time)


## Business sector aggregates -------------------------------------------------

dat_eurostat_na_ind <-
  dat_eurostat_na_ind_0 |>
  aggregate_activities(
    key      = key_activity_agg,
    ref_year = eurostat_ref_year,
    cp = "V", fp = "LR", pyp = "Y", additive = "_Z"
  ) |>
  arrange(geo, measure, activity, price_base, time)

save_dat(dat_eurostat_na_ind, overwrite = TRUE)


## Checks ---------------------------------------------------------------------

if (interactive()) {

  # Every activity should be there for every country and year.
  check_factor_levels(dat_eurostat_na_ind, drop_vars = "geo") |> print(n = Inf)

  # The parts should add up to the total in current prices, and the chained
  # BTNXL should track the sum of its components closely (it is not exactly the
  # sum, chain linking is not additive).
  dat_eurostat_na_ind |>
    filter(measure == "GVA", price_base == "V", geo == "FI") |>
    select(time, activity, values) |>
    pivot_wider(names_from = activity, values_from = values) |>
    mutate(diff_T = `_T` - (A + BTE + F + GTI + J + K + L + M_N + OTQ + RTU),
           diff_B = BTNXL - (BTE + F + GTI + J + K + M_N)) |>
    select(time, diff_T, diff_B) |>
    print(n = Inf)

}
