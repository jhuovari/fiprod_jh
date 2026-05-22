

library(tidyverse)
library(pttdatahaku)

devtools::load_all()

## Classifications

geo_ea <- c("BE", "DE", "EE", "IE", "EL", "ES", "FR", "IT", "CY", "LV",
            "LT", "LU", "MT", "NL", "AT", "PT", "SI", "SK", "FI", "HR")

usethis::use_data(geo_ea, overwrite = TRUE)

## update

update <- FALSE

if (update){

  source("data-raw/get_oecd_pdb.R")
  source("data-raw/get_exch.R")
}

dat_oecd_pdb_main <- load_dat("dat_oecd_pdb_main")

dat_oecd_pdb_ind <- load_dat("dat_oecd_pdb_ind")

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
