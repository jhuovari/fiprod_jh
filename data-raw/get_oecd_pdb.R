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

dat_oecd_pdb_main <-
  dat_oecd_pdb_main_0 |>
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
