

library(tidyverse)
library(pttdatahaku)
library(countrycode)

devtools::load_all()

## Classifications

geo_ea <- c("BE", "DE", "EE", "IE", "EL", "ES", "FR", "IT", "CY", "LV",
            "LT", "LU", "MT", "NL", "AT", "PT", "SI", "SK", "FI", "HR")

geos_oecd <- c("EA20",
               countrycode(c("FI", "SE", "NO", "DK", "BE", "NL", "AT", "PT",
                             "DE", "IT", "FR", "ES", "US", "JP", "UK"),
                           "eurostat", "iso3c"))

# Countries taken from Eurostat when the two sources are combined. Everything
# else (US, JP, UK) comes from the OECD; Eurostat no longer updates UK.
geos_eurostat <- c("EA20", geo_ea, "SE", "DK", "NO")


usethis::use_data(geo_ea, geos_oecd, geos_eurostat, overwrite = TRUE)

## update

update <- FALSE

if (update){

  source("data-raw/get_oecd_pdb.R")
  source("data-raw/get_eurostat_na.R")
  source("data-raw/get_exch.R")
}

dat_oecd_pdb_main <- load_dat("dat_oecd_pdb_main")

dat_oecd_pdb_ind <- load_dat("dat_oecd_pdb_ind")

dat_eurostat_na_ind <- load_dat("dat_eurostat_na_ind")

dat_ggdc_23 <- load_dat("dat_ggdc_23")

dat_ppp_va_ggdc_oecd <- load_dat("dat_ppp_va_ggdc_oecd")

dat_gva_ind <-
  dat_oecd_pdb_ind |>
  select(-unit_measure, -conversion_type, -var_id) |>
  filter_recode(price_base = c("cp" = "V", "fp_2020_lc" = "LR")) |>
  spread(price_base, values) |>
  left_join(select(filter(dat_ppp_va_ggdc_oecd, time == "2017-01-01"), -time),
            by = c("geo", "activity")) |>
  mutate(fp_2020_ppp17 = fp_2020_lc / ppp_va,
         fp_2020_xr17 = convert_currency(fp_2020_lc, geo, time, to = "USD", base_time = "2017-01-01")) |>
  pivot_longer(where(is.numeric), names_to = "vars", values_to = "values", names_transform = as_factor) |>
  spread(measure, values) |>
  mutate(HRS = GVA / GVAHRS,
         EMP = GVA / GVAEMP) |>
  pivot_longer(where(is.numeric), names_to = "measure", values_to = "values", names_transform = as_factor)

save_dat(dat_gva_ind, overwrite = TRUE)


## Eurostat and OECD combined ------------------------------------------------
#
# The OECD productivity database lags Eurostat by several months, so the EU/EEA
# countries are taken from Eurostat (data-raw/get_eurostat_na.R, which also
# builds the business sector aggregates BTNXL and GTNXL from the A*10
# industries) and the rest from the OECD.

# The OECD industry table publishes value added per person (GVAEMP) and per hour
# (GVAHRS) rather than the labour input itself, so persons and hours are backed
# out from the current price series. This puts the OECD data in the same shape
# as dat_eurostat_na_ind.
dat_oecd_na_ind_0 <-
  dat_oecd_pdb_ind |>
  filter(price_base %in% c("V", "LR"),
         measure %in% c("GVA", "GVAEMP", "GVAHRS")) |>
  select(time, geo, activity, measure, price_base, values) |>
  mutate(across(c(measure, price_base), as.character))

dat_oecd_na_ind <-
  dat_oecd_na_ind_0 |>
  filter(price_base == "V") |>
  select(-price_base) |>
  pivot_wider(names_from = measure, values_from = values) |>
  transmute(time, geo, activity,
            EMP = GVA / GVAEMP,
            HRS = GVA / GVAHRS) |>
  pivot_longer(c(EMP, HRS), names_to = "measure", values_to = "values") |>
  mutate(price_base = "_Z") |>
  bind_rows(filter(dat_oecd_na_ind_0, measure == "GVA")) |>
  mutate(measure = factor(measure, levels = c("GVA", "EMP", "HRS")),
         price_base = factor(price_base, levels = c("V", "LR", "Y", "_Z"))) |>
  arrange(geo, measure, activity, price_base, time)

# Eurostat reports value added in millions of national currency and employment
# in thousands, the OECD table may use another multiplier. `source_overlap`
# gives the ratio of the two on the countries both of them cover; set
# `oecd_scale` from it so that the levels of the combined data are comparable
# across countries. A growth correlation clearly below 1 means the two sources
# are not measuring the same thing and needs looking into.
oecd_scale <- c(GVA = 1, EMP = 1, HRS = 1)

source_overlap <-
  compare_sources(dat_eurostat_na_ind, dat_oecd_na_ind,
                  by = c("geo", "activity", "measure", "price_base"))

scale_summary <-
  source_overlap |>
  summarise(ratio = stats::median(ratio_median, na.rm = TRUE),
            growth_cor_min = min(growth_cor, na.rm = TRUE),
            n_series = n(),
            .by = measure)

print(scale_summary)

off_scale <- filter(scale_summary,
                    abs(log10(ratio / oecd_scale[as.character(measure)])) > 0.05)
if (nrow(off_scale)) {
  stop("Eurostat and OECD levels differ for ",
       paste0(off_scale$measure, " (x", signif(off_scale$ratio, 4), ")",
              collapse = ", "),
       ". Set `oecd_scale` in data-raw/data_main.R accordingly.")
}

dat_na_ind_comb <-
  combine_geo_sources(
    eurostat = select(dat_eurostat_na_ind, time, geo, activity, measure, price_base, values),
    oecd     = mutate(dat_oecd_na_ind,
                      values = values * unname(oecd_scale[as.character(measure)])),
    geos     = list(eurostat = geos_eurostat)
  ) |>
  mutate(across(c(geo, activity, measure, price_base, source), as_factor)) |>
  arrange(geo, measure, activity, price_base, time)

save_dat(dat_na_ind_comb, overwrite = TRUE)


# Labour productivity from the combined data, in the same form as dat_gva_ind.
# Previous year's prices (only published by Eurostat) are an input to the
# aggregation, not a result, and are left out here.
labour_comb <-
  dat_na_ind_comb |>
  filter(measure %in% c("EMP", "HRS")) |>
  select(time, geo, activity, source, measure, values) |>
  pivot_wider(names_from = measure, values_from = values)

dat_gva_ind_comb <-
  dat_na_ind_comb |>
  filter(measure == "GVA", price_base %in% c("V", "LR")) |>
  select(-measure) |>
  filter_recode(price_base = c("cp" = "V", "fp_2020_lc" = "LR")) |>
  spread(price_base, values) |>
  left_join(select(filter(dat_ppp_va_ggdc_oecd, time == "2017-01-01"), -time),
            by = c("geo", "activity")) |>
  mutate(fp_2020_ppp17 = fp_2020_lc / ppp_va,
         fp_2020_xr17 = convert_currency(fp_2020_lc, geo, time, to = "USD", base_time = "2017-01-01")) |>
  select(-ppp_va) |>
  pivot_longer(c(cp, fp_2020_lc, fp_2020_ppp17, fp_2020_xr17),
               names_to = "vars", values_to = "GVA", names_transform = as_factor) |>
  left_join(labour_comb, by = c("time", "geo", "activity", "source")) |>
  mutate(GVAEMP = GVA / EMP,
         GVAHRS = GVA / HRS) |>
  pivot_longer(c(GVA, EMP, HRS, GVAEMP, GVAHRS),
               names_to = "measure", values_to = "values", names_transform = as_factor)

save_dat(dat_gva_ind_comb, overwrite = TRUE)
