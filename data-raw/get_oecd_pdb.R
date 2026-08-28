## code to prepare productivity datasets

library(tidyverse)
library(countrycode)
library(OECD)



pdb_dataset <- "OECD.SDD.TPS,DSD_PDB@DF_PDB,"

## Main data

pdb_main_key <- oecd_make_filter(
  list(geos_oecd, "A", c("GVAEMP", "GVAHRS", "GDP", "GVA", "GDPPOP", "HRS", "POP", "HRSPOP"), c("_T", "BTNXL"), NULL, NULL, "N", NULL, NULL))

dat_oecd_pdb_main_0 <- get_dataset(
  dataset = pdb_dataset, filter = pdb_main_key)

dat_oecd_pdb_main_1 <-
  dat_oecd_pdb_main_0 |>
  oecd_clean_data(drop_vars = c("UNIT_MULT", "OBS_STATUS"),
                  vars = c(geo = "REF_AREA",
                           "measure"        = "MEASURE",
                           "activity"       = "ACTIVITY",
                           "unit_measure"   = "UNIT_MEASURE",
                           "price_base"     = "PRICE_BASE",
                           "conversion_type"= "CONVERSION_TYPE")) |>
  mutate(geo = as_factor(countrycode(geo, "iso3c", "eurostat", nomatch = NULL))) |>
  mutate(across(c(measure, activity, unit_measure, price_base, conversion_type),
                as.character))

# The database gives labour input for the whole economy only as ratios: value
# added per person (GVAEMP) and per hour (GVAHRS). The levels themselves are
# asked for above but not published, so they are backed out of the current price
# series here, which puts the table in the same shape as the Eurostat one.
#
# The division is done inside one currency conversion and gives the same head
# count and the same hours in either, so the result is stored once, as a
# quantity rather than a converted value: unit PS or H, no price base and no
# conversion type. National currency is preferred where it exists; the countries
# OECD publishes in PPP dollars only (the US) are covered by the PPP series.
pdb_main_labour <-
  dat_oecd_pdb_main_1 |>
  filter(price_base == "V", measure %in% c("GVA", "GVAEMP", "GVAHRS")) |>
  select(time, geo, activity, conversion_type, measure, values) |>
  pivot_wider(names_from = measure, values_from = values) |>
  transmute(time, geo, activity, conversion_type,
            EMP = GVA / GVAEMP,
            HRS = GVA / GVAHRS) |>
  pivot_longer(c(EMP, HRS), names_to = "measure", values_to = "values") |>
  filter(is.finite(values)) |>
  arrange(match(conversion_type, c("_Z", "PPP"))) |>
  distinct(time, geo, activity, measure, .keep_all = TRUE) |>
  transmute(time, geo, measure, activity,
            unit_measure = if_else(measure == "EMP", "PS", "H"),
            price_base = "_Z", conversion_type = "_Z", values)

# Should the database start publishing the levels, the published rows win.
dat_oecd_pdb_main <-
  bind_rows(dat_oecd_pdb_main_1, pdb_main_labour) |>
  distinct(time, geo, measure, activity, unit_measure, price_base,
           conversion_type, .keep_all = TRUE) |>
  mutate(across(c(measure, activity, unit_measure, price_base, conversion_type),
                as_factor)) |>
  unite("var_id", measure, unit_measure, price_base, conversion_type, sep = "-", remove = FALSE) |>
  mutate(var_id = as_factor(var_id)) |>
  arrange(geo, measure, activity, price_base, conversion_type, time)

save_dat(dat_oecd_pdb_main, overwrite = TRUE)

# check_factor_levels(dat_oecd_pdb_main, drop_vars = "geo") |> View()

## Industry data



pdb_ind_key <- oecd_make_filter(
  list(geos_oecd, "A", c("GVAEMP", "GVAHRS", "GVA", "EMP"), NULL, NULL, NULL, "N", NULL, "_Z"))

dat_oecd_pdb_ind_0 <- get_dataset(
  dataset = pdb_dataset, filter = pdb_ind_key)

dat_oecd_pdb_ind <-
  dat_oecd_pdb_ind_0 |>
  oecd_clean_data(drop_vars = c("UNIT_MULT", "OBS_STATUS"),
                  vars = c(geo = "REF_AREA",
                           "measure"        = "MEASURE",
                           "activity"       = "ACTIVITY",
                           "unit_measure"   = "UNIT_MEASURE",
                           "price_base"     = "PRICE_BASE",
                           "conversion_type"= "CONVERSION_TYPE")) |>
  mutate(geo = as_factor(countrycode(geo, "iso3c", "eurostat", nomatch = NULL))) |>
  unite("var_id", measure, unit_measure, price_base, conversion_type, sep = "-", remove = FALSE) |>
  mutate(var_id = as_factor(var_id))

save_dat(dat_oecd_pdb_ind, overwrite = TRUE)


## R&D data

rd_dataset <- "OECD.STI.STP,DSD_MSTI@DF_MSTI,"

pdb_rd_key <- oecd_make_filter(
  list(geos_oecd, "A", c("G", "T_TT", "GDP", "TOT_EMP"), NULL, c("V", "_Z"), "_Z"))

dat_oecd_pdb_rd_0 <- get_dataset(
  dataset = rd_dataset, filter = pdb_rd_key)

dat_oecd_pdb_rd <-
  dat_oecd_pdb_rd_0 |>
  oecd_clean_data(drop_vars = c("UNIT_MULT", "OBS_STATUS"),
                  vars = c(geo = "REF_AREA",
                           "measure"        = "MEASURE",
                           "unit_measure"   = "UNIT_MEASURE",
                           "price_base"     = "PRICE_BASE",
                           "transformation"= "TRANSFORMATION")) |>
  mutate(geo = as_factor(countrycode(geo, "iso3c", "eurostat", nomatch = NULL))) |>
  unite("var_id", measure, unit_measure, price_base, transformation, sep = "-", remove = FALSE) |>
  mutate(var_id = as_factor(var_id))

save_dat(dat_oecd_pdb_rd, overwrite = TRUE)
