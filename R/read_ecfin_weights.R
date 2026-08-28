#' Read ecfin weights from zip file
#'
#' Weights are in induvidual csv-files.
#'
#' @param zipfile A zip-file to read.
#' @param file_pre A first part of the filename inside zip-file.
#'
#' @export
#'
#' @import dplyr
#'

read_ecfin_weights <- function(zipfile, file_pre){

  w_files <- unzip(zipfile, list = TRUE)$Name %>%
    grep(file_pre, ., value = TRUE)

  y <- purrr::map_dfr(w_files, read_ecf_w_file, zipfile = zipfile)
  y

}


#' read-function
#'
#' @keywords internal
read_ecf_w_file <- function(file, zipfile = zipfile){
  w_file <- unzip(zipfile, file, junkpaths = TRUE, exdir = tempdir())
  y <- read.csv(w_file, check.names = FALSE) %>%
    # year is on the name of the first column
    mutate(time = as.numeric((names(.)[1]))) %>%
    rename(geo_base = 1) %>%
    gather(geo, weight, -geo_base, -time) %>%
    mutate(across(c("geo_base", "geo"), ~gsub("_", " ", .x))) |>
    mutate_at(c("geo_base", "geo"), ~countrycode::countrycode(., origin = "country.name.en",
                                                              destination = "eurostat",
                                                              custom_match = c("EA19" = "EA",
                                                                               "EA20" = "EA20",
                                                                               "EU27" = "EU"
                                                                               # ,
                                                                               # "37" = "IC37",
                                                                               # "42" = "gr42"
                                                              )
    ))
  y
}
