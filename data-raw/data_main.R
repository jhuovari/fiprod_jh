

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
  source("data-raw/get_eurostat_gdp.R")
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
oecd_scale <- c(GVA = 1, EMP = 1000, HRS = 1000)

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


## Whole economy: Eurostat and OECD combined ---------------------------------
#
# The same combination for dat_oecd_pdb_main. GDP and population come from
# nama_10_gdp (data-raw/get_eurostat_gdp.R); value added, employment and hours
# for the whole economy (`_T`) and the business sector (`BTNXL`) are already in
# dat_eurostat_na_ind and are reused here. (nama_10_gdp's B1G is the same series
# as the industry table's `_T` value added and is kept only as a cross-check.)

dat_eurostat_na_gdp <- load_dat("dat_eurostat_na_gdp")

dat_eurostat_main_lvl <-
  bind_rows(
    filter(dat_eurostat_na_gdp, measure %in% c("GDP", "POP")),
    filter(dat_eurostat_na_ind,
           activity %in% c("_T", "BTNXL"),
           measure %in% c("GVA", "EMP", "HRS"))
  ) |>
  mutate(across(c(measure, activity, unit_measure, price_base), as.character))

# The ratio measures OECD publishes. Population only exists for `_T`, so the
# inner join drops GDPPOP and HRSPOP for BTNXL by itself. The units are the ones
# OECD uses: a per person series is `_PS` and a per hour series `_H`, so GDP per
# capita is XDC_PS (and USD_PPP_PS once converted), not XDC.
ratio_spec <- tibble::tribble(
  ~out_measure, ~num_measure, ~den_measure, ~out_unit, ~price_bases,
  "GDPPOP",     "GDP",        "POP",        "XDC_PS",  c("V", "LR"),
  "GVAEMP",     "GVA",        "EMP",        "XDC_PS",  c("V", "LR"),
  "GVAHRS",     "GVA",        "HRS",        "XDC_H",   c("V", "LR"),
  "HRSPOP",     "HRS",        "POP",        "H_PS",    "_Z"
)

# Eurostat does not use one multiplier throughout: value added and GDP are in
# millions of national currency, employment, hours and population in thousands.
# Dividing one by the other therefore does not yet give a per person or per hour
# figure, and the ratio is rescaled so that it comes out in OECD's units.
eurostat_mult <- c(GDP = 1e6, GVA = 1e6, EMP = 1e3, HRS = 1e3, POP = 1e3)

make_ratio <- function(out_measure, num_measure, den_measure, out_unit, price_bases) {
  mult <- unname(eurostat_mult[num_measure] / eurostat_mult[den_measure])

  num <- dat_eurostat_main_lvl |>
    filter(measure == num_measure, price_base %in% price_bases) |>
    select(time, geo, activity, price_base, num = values)
  den <- dat_eurostat_main_lvl |>
    filter(measure == den_measure) |>
    select(time, geo, activity, den = values)

  num |>
    inner_join(den, by = c("time", "geo", "activity")) |>
    transmute(time, geo,
              measure = out_measure, activity,
              unit_measure = out_unit, price_base,
              values = mult * num / den)
}

dat_eurostat_main <-
  bind_rows(dat_eurostat_main_lvl, purrr::pmap_dfr(ratio_spec, make_ratio)) |>
  arrange(geo, measure, activity, price_base, time)

# OECD side in national currency. OECD publishes no market exchange rate series
# for the US, because for the US the PPP series already are in USD.
dat_oecd_main_nac <-
  dat_oecd_pdb_main |>
  filter(conversion_type %in% c("_Z", "PPP")) |>
  mutate(unit_measure = sub("^USD_PPP", "XDC", as.character(unit_measure)),
         across(c(geo, measure, activity, price_base, conversion_type), as.character)) |>
  filter(conversion_type == "_Z" | geo == "US") |>
  arrange(match(conversion_type, c("_Z", "PPP"))) |>
  distinct(time, geo, measure, activity, unit_measure, price_base, .keep_all = TRUE) |>
  select(time, geo, measure, activity, unit_measure, price_base, values)

# Multipliers, as for the industry data: OECD counts persons and hours in
# millions, Eurostat in thousands, so the OECD levels are multiplied by a
# thousand. Population is assumed to follow employment and hours; if it does not,
# the check below says so. The ratio measures are already built in OECD's own
# units above, so the OECD side of them needs no conversion.
oecd_scale_main <- c(GDP = 1, GVA = 1, EMP = 1000, HRS = 1000, POP = 1000)
oecd_scale_main <- c(
  oecd_scale_main,
  GDPPOP = 1, HRSPOP = 1, GVAEMP = 1, GVAHRS = 1
)

source_overlap_main <-
  compare_sources(dat_eurostat_main, dat_oecd_main_nac,
                  by = c("geo", "activity", "measure", "unit_measure", "price_base"))

scale_summary_main <-
  source_overlap_main |>
  summarise(ratio = stats::median(ratio_median, na.rm = TRUE),
            growth_cor_min = min(growth_cor, na.rm = TRUE),
            n_series = n(),
            .by = measure)

print(scale_summary_main)

off_scale_main <- filter(scale_summary_main,
                         abs(log10(ratio / oecd_scale_main[as.character(measure)])) > 0.05)
if (nrow(off_scale_main)) {
  stop("Eurostat and OECD levels differ for ",
       paste0(off_scale_main$measure, " (x", signif(off_scale_main$ratio, 4), ")",
              collapse = ", "),
       ". Set `oecd_scale_main` in data-raw/data_main.R accordingly.")
}

dat_gdp_main_nac <-
  combine_geo_sources(
    eurostat = dat_eurostat_main,
    oecd     = mutate(dat_oecd_main_nac,
                      values = values * unname(oecd_scale_main[measure])),
    geos     = list(eurostat = geos_eurostat)
  )

# PPP conversion. Eurostat has no PPP series in USD, so OECD's own conversion
# factors are used: national currency per PPP dollar, read off the two versions
# of the same OECD series. For the fixed price series the factor is the base
# year PPP and does not vary over time; for current prices it does, and the last
# available factor is carried forward to the years Eurostat already has but OECD
# does not. For the US the factor is 1 by construction.
key_unit_ppp <- c("XDC" = "USD_PPP", "XDC_PS" = "USD_PPP_PS", "XDC_H" = "USD_PPP_H")

ppp_factor <-
  dat_oecd_pdb_main |>
  filter(conversion_type %in% c("_Z", "PPP")) |>
  mutate(unit_measure = sub("^USD_PPP", "XDC", as.character(unit_measure)),
         across(c(geo, measure, activity, price_base, conversion_type), as.character)) |>
  filter(unit_measure %in% names(key_unit_ppp)) |>
  select(time, geo, measure, activity, unit_measure, price_base, conversion_type, values) |>
  pivot_wider(names_from = conversion_type, values_from = values) |>
  mutate(ppp = if_else(geo == "US", 1, `_Z` / PPP)) |>
  select(-`_Z`, -PPP) |>
  arrange(time) |>
  group_by(geo, measure, activity, unit_measure, price_base) |>
  tidyr::fill(ppp, .direction = "downup") |>
  ungroup()

dat_gdp_main <-
  dat_gdp_main_nac |>
  mutate(conversion_type = "_Z") |>
  bind_rows(
    dat_gdp_main_nac |>
      inner_join(ppp_factor,
                 by = c("time", "geo", "measure", "activity", "unit_measure",
                        "price_base")) |>
      transmute(time, geo, measure, activity,
                unit_measure = unname(key_unit_ppp[unit_measure]),
                price_base, values = values / ppp, source,
                conversion_type = "PPP")
  ) |>
  mutate(across(c(geo, measure, activity, unit_measure, price_base,
                  conversion_type, source), as_factor)) |>
  unite("var_id", measure, unit_measure, price_base, conversion_type,
        sep = "-", remove = FALSE) |>
  mutate(var_id = as_factor(var_id)) |>
  select(time, geo, measure, activity, unit_measure, price_base, conversion_type,
         var_id, source, values) |>
  arrange(geo, measure, activity, price_base, conversion_type, time)

# A country outside the OECD main table has no PPP factor to borrow, so it gets
# national currency series only. That is every Eurostat country not in
# `geos_oecd`, and it is worth knowing before drawing a level comparison.
no_ppp <-
  dat_gdp_main |>
  summarise(has_ppp = any(conversion_type == "PPP"), .by = geo) |>
  filter(!has_ppp)

if (nrow(no_ppp)) {
  message("National currency only, no OECD PPP factor: ",
          paste(no_ppp$geo, collapse = ", "), ".")
}

save_dat(dat_gdp_main, overwrite = TRUE)
