# Exchange rates from eurostat

library(eurostat)

currencies <- c(AU = "AUD", CA = "CAD", US = "USD", JP = "JPY", NZ = "NZD", CH = "CHF", DK = "DKK", SE = "SEK", NO = "NOK", UK = "GBP")

exh_eur_euro <-
  list(time = 1971:lubridate::year(Sys.time()),
       geo = geo_ea,
       currency = "EUR",
       values = 1) |>
  expand.grid()

exh_eur_a <- get_eurostat("ert_bil_eur_a", filters = list(statinfo = "AVG", currency = currencies), time_format = "num") %>%
  select(time, currency, values) %>%
  mutate(geo = recode(currency, !!!purrr::set_names(names(currencies), currencies))) |>
  bind_rows(exh_eur_euro) |>
  mutate(time = as.Date(paste0(time, "-01-01")))



usethis::use_data(exh_eur_a, overwrite = TRUE)
