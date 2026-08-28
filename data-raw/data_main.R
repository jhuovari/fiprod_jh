

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


# Peer group of the price competitiveness indicators, the 17 countries ficomp
# weighted against. Eurostat's national accounts cover 15 of them, including
# Switzerland through EFTA. The US and Japan are not there and come from the
# OECD productivity database instead, which carries the unit labour cost and its
# parts but no exports or imports - so they have no terms of trade adjustment.
geos_comp_es   <- c("BE", "DK", "DE", "IE", "ES", "FR", "IT", "NL",
                    "AT", "FI", "SE", "NO", "PT", "EL", "CH")
geos_comp_oecd <- c("US", "JP")
geos_comp      <- c(geos_comp_es, geos_comp_oecd)

# Index base year of the competitiveness indicators, as in ficomp.
comp_base_year <- 2010


usethis::use_data(geo_ea, geos_oecd, geos_eurostat,
                  geos_comp, geos_comp_es, geos_comp_oecd, comp_base_year,
                  overwrite = TRUE)

## update

update <- FALSE

if (update){

  source("data-raw/get_oecd_pdb.R")
  source("data-raw/get_oecd_lfs.R")
  source("data-raw/get_eurostat_na.R")
  source("data-raw/get_eurostat_gdp.R")
  source("data-raw/get_eurostat_ulc.R")
  source("data-raw/get_exch.R")
}

dat_oecd_pdb_main <- load_dat("dat_oecd_pdb_main")

dat_oecd_pdb_ind <- load_dat("dat_oecd_pdb_ind")

dat_oecd_wap <- load_dat("dat_oecd_wap")

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

# Eurostat does not use one multiplier throughout: value added and GDP are in
# millions of national currency, employment, hours and population in thousands.
# Dividing one by the other therefore does not yet give a per person or per hour
# figure, so every ratio measure below is rescaled by the two multipliers and
# comes out in OECD's units. The combined tables themselves stay in Eurostat
# units, which is what `oecd_scale` converts the OECD side to.
eurostat_mult <- c(GDP = 1e6, GVA = 1e6, EMP = 1e3, HRS = 1e3, POP = 1e3)

# `source_overlap` gives the ratio of the two sources on the countries both of
# them cover; set `oecd_scale` from it so that the levels of the combined data
# are comparable across countries. A growth correlation clearly below 1 means
# the two sources are not measuring the same thing and needs looking into.
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
  # value added is in millions and labour input in thousands, so the ratios need
  # the two multipliers to come out per person and per hour
  mutate(GVAEMP = unname(eurostat_mult["GVA"] / eurostat_mult["EMP"]) * GVA / EMP,
         GVAHRS = unname(eurostat_mult["GVA"] / eurostat_mult["HRS"]) * GVA / HRS) |>
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

# Employment and hours are not published for the whole economy and are backed
# out of GVAEMP and GVAHRS already in data-raw/get_oecd_pdb.R, so they arrive
# here like any other measure. They come out in OECD's own units - value added
# is in millions, so persons and hours are too - and `oecd_scale_main` puts them
# on Eurostat's thousands below.
#
# OECD's other route to the same hours is HRSPOP times POP: hours per head of
# population multiplied by the population. Hours are in millions and population
# in thousands, hence the factor of a thousand. The two have to agree; only the
# whole economy is covered, since HRSPOP has no business sector counterpart.
hrs_check <-
  inner_join(
    dat_oecd_main_nac |>
      filter(measure == "HRS", activity == "_T") |>
      select(time, geo, derived = values),
    dat_oecd_main_nac |>
      filter(measure %in% c("HRSPOP", "POP"), activity == "_T") |>
      select(time, geo, measure, values) |>
      pivot_wider(names_from = measure, values_from = values) |>
      transmute(time, geo, published = HRSPOP * POP / 1000),
    by = c("time", "geo")
  ) |>
  filter(is.finite(derived), is.finite(published), published != 0)

if (!nrow(hrs_check)) {
  warning("No hours to check in the OECD main table. Has data-raw/get_oecd_pdb.R ",
          "been run since it started deriving EMP and HRS?", call. = FALSE)
} else {
  hrs_ratio <- stats::median(hrs_check$derived / hrs_check$published, na.rm = TRUE)
  if (abs(log10(hrs_ratio)) > 0.005) {
    warning("Hours derived as GVA / GVAHRS and hours as HRSPOP * POP differ ",
            "by a factor of ", signif(hrs_ratio, 4),
            " in the OECD main table.", call. = FALSE)
  }
}

# Multipliers, as for the industry data: OECD counts persons and hours in
# millions, Eurostat in thousands, so the OECD levels are multiplied by a
# thousand. Population is assumed to follow employment and hours; if it does not,
# the check below says so. The ratio measures are already built in OECD's own
# units above, so the OECD side of them needs no conversion.
oecd_scale_main <- c(GDP = 1, GVA = 1, EMP = 1000, HRS = 1000, POP = 1)
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

dat_gdp_main_nac_0 <-
  combine_geo_sources(
    eurostat = dat_eurostat_main,
    oecd     = mutate(dat_oecd_main_nac,
                      values = values * unname(oecd_scale_main[measure])),
    geos     = list(eurostat = geos_eurostat)
  )

## Working age population -----------------------------------------------------
#
# The 15 to 64 year olds, from the OECD labour force survey database
# (data-raw/get_oecd_lfs.R). Neither productivity database carries it, so it is
# not part of the source combination above: it is added on top, for the
# countries the LFS query covers, and marked with a source of its own.
#
# That query drops the table's unit multiplier, so nothing in the data says
# whether a value counts persons or thousands of persons. The share of 15 to 64
# year olds in the population is between 55 and 75 per cent in every OECD
# country, which is far enough from a factor of ten to pin the multiplier down.
wap_0 <-
  dat_oecd_wap |>
  mutate(across(c(geo, measure, activity, age), as.character)) |>
  filter(measure == "WAP", activity == "_T", age == "Y15T64") |>
  select(time, geo, values)

wap_share <-
  wap_0 |>
  inner_join(filter(dat_gdp_main_nac_0, measure == "POP", activity == "_T") |>
               select(time, geo, pop = values),
             by = c("time", "geo")) |>
  filter(is.finite(values), is.finite(pop), pop != 0) |>
  summarise(share = stats::median(values / pop)) |>
  pull(share)

# a power of ten, so that the share lands where a working age share belongs
wap_mult <- 10^round(log10(0.65 / wap_share))

message("Working age population scaled by ", format(wap_mult, scientific = FALSE),
        "; share of the population ", signif(100 * wap_share * wap_mult, 3), " %.")

if (!is.finite(wap_share) ||
    !dplyr::between(wap_share * wap_mult, 0.55, 0.75)) {
  stop("The working age population is ", signif(100 * wap_share * wap_mult, 3),
       " % of the population after scaling by ",
       format(wap_mult, scientific = FALSE),
       ", which is not a working age share. Check the units of dat_oecd_wap ",
       "against POP in data-raw/data_main.R.")
}

dat_wap_main <-
  wap_0 |>
  transmute(time, geo,
            measure = "WAP", activity = "_T",
            unit_measure = "PS", price_base = "_Z",
            values = values * wap_mult,
            source = "oecd_lfs")

no_wap <- setdiff(unique(dat_gdp_main_nac_0$geo), unique(dat_wap_main$geo))
if (length(no_wap)) {
  message("No working age population, outside the OECD LFS query: ",
          paste(sort(no_wap), collapse = ", "), ".")
}

dat_gdp_main_nac <- bind_rows(dat_gdp_main_nac_0, dat_wap_main)

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


# The two combined tables build value added per hour by different routes, so
# they are a check on each other's unit multipliers: local currency, 2020
# prices, whole economy and business sector should give the same number.
ratio_check <-
  inner_join(
    dat_gva_ind_comb |>
      filter(measure == "GVAHRS", vars == "fp_2020_lc",
             activity %in% c("_T", "BTNXL")) |>
      select(time, geo, activity, ind = values),
    dat_gdp_main |>
      filter(measure == "GVAHRS", price_base == "LR", conversion_type == "_Z",
             activity %in% c("_T", "BTNXL")) |>
      select(time, geo, activity, main = values),
    by = c("time", "geo", "activity")
  ) |>
  filter(is.finite(ind), is.finite(main), ind != 0) |>
  mutate(ratio = main / ind)

# A multiplier that is out of step shifts every observation by the same factor,
# so the median is the statistic to look at; single countries can differ a
# little because the OECD main and industry tables are separate vintages.
ratio_median <- stats::median(ratio_check$ratio, na.rm = TRUE)

if (nrow(ratio_check) && abs(log10(ratio_median)) > 0.01) {
  warning("dat_gva_ind_comb and dat_gdp_main disagree on GVAHRS by a factor of ",
          signif(ratio_median, 4),
          ". Check `eurostat_mult` against the units of the source tables.",
          call. = FALSE)
}


## Price competitiveness ------------------------------------------------------
#
# The core of the old ficomp package (https://github.com/pttry/ficomp), which
# the productivity board's competitiveness chapter was built on. Nominal and
# real unit labour costs, their decomposition into productivity, compensation
# and the exchange rate, and each of them relative to the peer group with the
# ECFIN trade weights.
#
# The peer group is ficomp's 17 countries. 15 of them come from Eurostat
# (`geos_comp_es`); the US and Japan come from the OECD productivity database
# (`geos_comp_oecd`), which publishes the unit labour cost and its parts but no
# exports or imports. They therefore have no terms of trade adjustment and no
# unadjusted measure, and those indicators are weighted over the 15 Eurostat
# countries only. The `peers` column of the result says which set was used.

dat_eurostat_ulc <- load_dat("dat_eurostat_ulc")

# Every peer has to have the base year, or the index is undefined for it and the
# weighted relatives of all the others turn into NA.
ulc_base_missing <-
  dat_eurostat_ulc |>
  filter(geo %in% geos_comp_es, lubridate::year(time) == comp_base_year) |>
  summarise(across(c(B1GQ__CLV20_MNAC, D1__CP_MNAC, D1__CP_MEUR,
                     EMP_DC__THS_PER, SAL_DC__THS_PER),
                   ~ sum(!is.finite(.x))),
            .by = geo) |>
  filter(if_any(where(is.numeric), ~ .x > 0))

if (nrow(ulc_base_missing)) {
  stop("No ", comp_base_year, " observation for: ",
       paste(ulc_base_missing$geo, collapse = ", "),
       ". Pick another `comp_base_year` or drop the country from `geos_comp`.")
}

# The indicators. `nulc_aper` is the entrepreneur adjusted measure the board
# reports: compensation per employee against output per employed person.
dat_ulc_lvl <-
  dat_eurostat_ulc |>
  filter(geo %in% geos_comp_es) |>
  mutate(geo = as.character(geo)) |>
  arrange(time) |>
  mutate(
    # Terms of trade adjusted GDP volume: exports revalued at import prices.
    B1GQA__CLV20_MNAC = gdp_trading_gain(
      gdp = B1GQ__CLV20_MNAC,
      exports = P6__CLV20_MNAC, exports_cp = P6__CP_MNAC,
      imports = P7__CLV20_MNAC, imports_cp = P7__CP_MNAC),

    # Nominal unit labour costs, whole economy
    nulc     = ind_ulc(D1__CP_MNAC, B1GQ__CLV20_MNAC, time = time, baseyear = comp_base_year),
    nulc_eur = ind_ulc(D1__CP_MEUR, B1GQ__CLV20_MNAC, time = time, baseyear = comp_base_year),
    nulc_va  = ind_ulc(D1__CP_MNAC, B1G__CLV20_MNAC,  time = time, baseyear = comp_base_year),

    # Entrepreneur adjusted, in own and in common currency
    nulc_aper = ind_ulc(D1__CP_MNAC, B1GQ__CLV20_MNAC,
                        SAL_DC__THS_PER, EMP_DC__THS_PER, time, comp_base_year),
    nulc_aper_eur = ind_ulc(D1__CP_MEUR, B1GQ__CLV20_MNAC,
                            SAL_DC__THS_PER, EMP_DC__THS_PER, time, comp_base_year),

    # ... and against the terms of trade adjusted output
    nulc_aper_atot = ind_ulc(D1__CP_MNAC, B1GQA__CLV20_MNAC,
                             SAL_DC__THS_PER, EMP_DC__THS_PER, time, comp_base_year),
    nulc_aper_eur_atot = ind_ulc(D1__CP_MEUR, B1GQA__CLV20_MNAC,
                                 SAL_DC__THS_PER, EMP_DC__THS_PER, time, comp_base_year),

    # Real unit labour cost: the nominal one deflated by the GDP deflator, i.e.
    # the labour share of output
    rulc_aper = rebase_index(
      nulc_aper / (B1GQ__CP_MNAC / B1GQ__CLV20_MNAC), time, comp_base_year),

    # The three parts the nominal common currency measure decomposes into
    lp_ind       = rebase_index(B1GQ__CLV20_MNAC / EMP_DC__THS_PER, time, comp_base_year),
    d1_per_ind   = rebase_index(D1__CP_MNAC / SAL_DC__THS_PER, time, comp_base_year),
    exch_eur_ind = rebase_index(D1__CP_MNAC / D1__CP_MEUR, time, comp_base_year),

    .by = geo
  )

## The US and Japan from the OECD productivity database ----------------------
#
# OECD publishes GDP per employed person and labour cost per employee, which is
# the same construction as the Eurostat side: cost per employee against output
# per employed person. Building the unit labour cost from those two rather than
# taking OECD's published ULCE keeps the decomposition identity
# `nulc_aper = 100 * d1_per_ind / lp_ind` exact for every country; the check
# below compares the result with ULCE anyway.

dat_oecd_pdb_ulc <- load_dat("dat_oecd_pdb_ulc")
dat_oecd_ulcq    <- load_dat("dat_oecd_ulcq")

# Annual OECD levels, one column per measure and price base
oecd_lvl_long <-
  dat_oecd_pdb_ulc |>
  filter(geo %in% geos_comp_oecd, measure %in% c("GDPEMP", "LCEMP"),
         price_base %in% c("V", "LR")) |>
  transmute(time = as.Date(paste0(time, "-01-01")),
            geo = as.character(geo),
            series = paste(measure, price_base, sep = "_"),
            values)

# The annual table runs a year or two behind for the US and Japan, while the
# quarterly one is current. Take the annual growth of the quarterly series and
# carry the annual level forward with it. Quarterly volumes carry the price base
# `Q` where the annual table has `LR`.
key_q_price <- c("V" = "V", "LR" = "Q")

# The growth is taken over the quarters the two years have in common, so a year
# that is only half published is compared with the same half of the year before
# rather than with a full year. For a complete year this is exactly the growth
# of the annual average. The series are seasonally adjusted, so a part year
# average is a fair reading of the level.
ulc_min_quarters <- 2

ulcq_q <-
  dat_oecd_ulcq |>
  filter(geo %in% geos_comp_oecd, measure %in% c("GDPEMP", "LCEMP"),
         price_base %in% key_q_price) |>
  transmute(geo = as.character(geo),
            measure = as.character(measure),
            price_base = as.character(price_base),
            year = lubridate::year(time),
            q = lubridate::quarter(time),
            values)

ulcq_change <-
  inner_join(
    ulcq_q,
    mutate(ulcq_q, year = year + 1L) |> rename(values_prev = values),
    by = c("geo", "measure", "price_base", "year", "q")
  ) |>
  summarise(n_q = n(),
            change = mean(values) / mean(values_prev) - 1,
            .by = c(geo, measure, price_base, year)) |>
  filter(n_q >= ulc_min_quarters) |>
  transmute(time = as.Date(paste0(year, "-01-01")), geo,
            series = paste(measure,
                           names(key_q_price)[match(price_base, key_q_price)],
                           sep = "_"),
            change, n_q)

missing_q <- setdiff(unique(oecd_lvl_long$series), unique(ulcq_change$series))
if (length(missing_q)) {
  warning("No quarterly counterpart for ", paste(missing_q, collapse = ", "),
          "; those series are not extended. Check `key_q_price` against the ",
          "price bases in dat_oecd_ulcq.", call. = FALSE)
}

oecd_extended <-
  oecd_lvl_long |>
  full_join(ulcq_change, by = c("time", "geo", "series")) |>
  arrange(time) |>
  mutate(extended = is.na(values),
         values = extend_with_change(values, change, time),
         extended = extended & !is.na(values),
         .by = c(geo, series)) |>
  select(-change)

# Which country years rest on the quarterly extension rather than on the annual
# national accounts
ulc_extended_years <-
  oecd_extended |>
  filter(extended) |>
  summarise(vuodet = paste(sort(unique(lubridate::year(time))), collapse = ", "),
            min_neljanneksia = min(n_q, na.rm = TRUE),
            .by = geo)

if (nrow(ulc_extended_years)) {
  message("Extended with quarterly growth:")
  print(ulc_extended_years)
}

dat_ulc_oecd <-
  oecd_extended |>
  select(-n_q) |>
  pivot_wider(names_from = series, values_from = c(values, extended)) |>
  # a country year rests on the extension if any of its inputs did
  mutate(extended = if_any(starts_with("extended_"), ~ .x %in% TRUE)) |>
  select(-starts_with("extended_")) |>
  rename_with(~ sub("^values_", "", .x)) |>
  # National currency per euro, the same definition as D1__CP_MNAC / D1__CP_MEUR
  left_join(transmute(exh_eur_a, time, geo = as.character(geo), xr = values),
            by = c("time", "geo")) |>
  arrange(time) |>
  mutate(
    lp_ind        = rebase_index(GDPEMP_LR, time, comp_base_year),
    d1_per_ind    = rebase_index(LCEMP_V, time, comp_base_year),
    nulc_aper     = rebase_index(LCEMP_V / GDPEMP_LR, time, comp_base_year),
    # Real unit labour cost is the nominal one deflated by the GDP deflator,
    # GDPEMP_V / GDPEMP_LR, which leaves the labour share LCEMP_V / GDPEMP_V
    rulc_aper     = rebase_index(LCEMP_V / GDPEMP_V, time, comp_base_year),
    exch_eur_ind  = rebase_index(xr, time, comp_base_year),
    nulc_aper_eur = 100 * nulc_aper / exch_eur_ind,
    .by = geo
  )


## The two sides together ----------------------------------------------------

# Available for every peer, weighted over all 17
comp_vars_all <- c("nulc_aper", "nulc_aper_eur", "rulc_aper",
                   "lp_ind", "d1_per_ind", "exch_eur_ind")

# Need exports, imports or the unadjusted cost, so Eurostat only, weighted
# over the 15
comp_vars_es <- c("nulc", "nulc_eur", "nulc_va",
                  "nulc_aper_atot", "nulc_aper_eur_atot")

dat_ulc_all <-
  bind_rows(
    select(dat_ulc_lvl, time, geo, all_of(c(comp_vars_all, comp_vars_es))) |>
      mutate(extended = FALSE),
    select(dat_ulc_oecd, time, geo, extended, all_of(comp_vars_all))
  ) |>
  mutate(geo = factor(geo, levels = geos_comp))

# Relative to the peers: each country's index divided by the trade weighted
# geometric mean of the others, times 100. Above 100 means costs have risen more
# than in the peer countries since the base year.
ulc_rel <- function(d, vars, geos) {
  d |>
    filter(geo %in% geos) |>
    select(time, geo, all_of(vars)) |>
    group_by(time) |>
    mutate(across(all_of(vars),
                  ~ weight_index2(.x, geo, time, geos = geos,
                                  weight_df = weights_ecfin37))) |>
    ungroup() |>
    pivot_longer(-c(time, geo), names_to = "vars", values_to = "rel") |>
    mutate(peers = length(geos))
}

dat_ulc_comp <-
  dat_ulc_all |>
  pivot_longer(-c(time, geo, extended), names_to = "vars", values_to = "values") |>
  # the OECD countries simply do not have the Eurostat only indicators
  filter(!(geo %in% geos_comp_oecd & vars %in% comp_vars_es)) |>
  left_join(
    bind_rows(ulc_rel(dat_ulc_all, comp_vars_all, geos_comp),
              ulc_rel(dat_ulc_all, comp_vars_es, geos_comp_es)),
    by = c("time", "geo", "vars")
  ) |>
  mutate(vars = factor(vars, levels = c(comp_vars_all, comp_vars_es))) |>
  select(time, geo, vars, values, rel, peers, extended) |>
  arrange(geo, vars, time)

save_dat(dat_ulc_comp, overwrite = TRUE)


# OECD also publishes the unit labour cost ready made (ULCE). Ours is built from
# the two parts, so the two should agree; a gap means the definitions differ.
ulce_check <-
  dat_oecd_pdb_ulc |>
  filter(geo %in% geos_comp_oecd, measure == "ULCE", unit_measure == "IX") |>
  transmute(time = as.Date(paste0(time, "-01-01")),
            geo = as.character(geo), ulce = values) |>
  inner_join(
    dat_ulc_comp |>
      filter(vars == "nulc_aper", geo %in% geos_comp_oecd) |>
      transmute(time, geo = as.character(geo), nulc = values),
    by = c("time", "geo")
  ) |>
  filter(is.finite(ulce), is.finite(nulc), ulce != 0) |>
  mutate(ulce = rebase_index(ulce, time, comp_base_year), .by = geo) |>
  summarise(ero_pros = 100 * (stats::median(nulc / ulce) - 1),
            suurin_pros = 100 * max(abs(nulc / ulce - 1)),
            .by = geo)

message("nulc_aper vs OECD ULCE:")
print(ulce_check)
