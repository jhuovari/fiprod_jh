## code to get working age population data from OECD LFS database

library(tidyverse)
library(countrycode)
library(OECD)  # OECD package should be => 0.3.0, currently form github pak::pak("expersso/OECD")



wap_dataset <- "OECD.SDD.TPS,DSD_LFS@DF_IALFS_WAP_Q,1.0"

wap_key <- oecd_make_filter(
  list(
    geos_oecd,  # REF_AREA
    "WAP",      # MEASURE
    NULL,       # ADJUSTMENT
    NULL,       # TRANSFORMATION
    "Y",        # SEX (Y = total)
    "_T",       # ACTIVITY
    "Y15T64",   # AGE
    NULL,       # UNIT_MEASURE
    "A"         # FREQ
  )
)

dat_oecd_wap_0 <- get_dataset(
  dataset = wap_dataset,
  filter  = wap_key
)

dat_oecd_wap <-
  dat_oecd_wap_0 |>
  oecd_clean_data(
    drop_vars = c("UNIT_MULT", "OBS_STATUS", "SEX"),
    vars = c(
      geo        = "REF_AREA",
      "measure"  = "MEASURE",
      "activity" = "ACTIVITY",
      "age"      = "AGE"
    )
  ) |>
  mutate(geo = as_factor(countrycode(geo, "iso3c", "eurostat", nomatch = NULL)))

save_dat(dat_oecd_wap, overwrite = TRUE)
