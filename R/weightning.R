#' Mass weighting
#'
#' Weight all variables except.
#'
#' @param .data A tbl.
#' @param weitht_df A weigthing data.frame in long form. Should have time, geo_base and geo columns.
#' @param geo A vector to indicate countries.
#' @param except Variables to exclude, a character vector.
#' @param ar Variables to include, a character vector.
#' @param time A year (or date which is converted to a year) for weights.
#' @param weitht_df A weigthing data.frame in long form. Should have time, geo_base and geo columns.
#'
#' @export
#' @import dplyr
#'
weight_all <- function(.data, geo, time, except, weight_df){
  w_name <- deparse1(substitute(weight_df))
  fun_name <- paste0("rel", gsub("weights", "", x = w_name))
  fun_list <- list(rel = ~weight_index(., geo = geo, time = time, weight_df = weight_df))
  names(fun_list) <- fun_name

  y <- group_by(.data, time, .add = TRUE) %>%
    mutate_at(vars(-matches(except)),
              .funs = fun_list) %>%
    ungroup()

  y
}

weight_all2 <- function(.data, .geo, .time, ..., .except, .weight_df){
  w_name <- deparse1(substitute(.weight_df))
  fun_name <- paste0("rel", gsub("weights", "", x = w_name))
  args <-  rlang::quos(geo = .geo, time = .time, weight_df = .weight_df, ..., .named = TRUE)

  y <- group_by(.data, time, .add = TRUE) %>%
    mutate(across(-matches(.except),
                  weight_index, !!! args,
                  .names = paste0("{col}_rel_", fun_name))) %>%
    ungroup()

  y
}

#' @describeIn weight_all To spesify variables to weight

weight_at <- function(.data, geo, time, at, weight_df){
  w_name <- deparse1(substitute(weight_df))
  fun_name <- paste0("rel", gsub("weights", "", x = w_name))
  fun_list <- list(rel = ~weight_index(., geo = geo, time = time, weight_df = weight_df))
  names(fun_list) <- fun_name

  y <- group_by(.data, time, .add = TRUE) %>%
    mutate_at(tidyselect::all_of(setNames(at, at)), .funs = fun_list) %>%
    ungroup()

  y
}




#' Weighted geometric mean
#'
#' Calculate weighted geometric mean
#'
#' @param x a numeric vector
#' @param w a numeric vector for weights
#' @param na.rm A logical. Should missing x values be removed?
#'
#' @export
#' @examples
#' x <- c(1,2,3, NA)
#' w <- c(0.25,0.5,0.25, NA)
#' weighted_gmean(x, w, na.rm = TRUE)
weighted_gmean <- function(x, w, na.rm = FALSE) {
  if (na.rm) w[is.na(x)] <- 0
  y <- prod(x^prop.table(w))
  y
}


#' Calculate weighted index
#'
#' Uses weights from a data frame.
#'
#' weight_index2 allowes spesifing subset of observatios (geos) for weighting.
#'
#' @param x A vector to weight.
#' @param geo A vector to indicate countries.
#' @param time A year (or date which is converted to a year) for weights.
#' @param geos A string vector of geos to weight over.
#' @param weight_df A weigthing data.frame in long form. Should have time, geo_base and geo columns.
#' @param nearest A logical whether to use nearest year for weight table.
#' @param na.rm A logical. Should missing values be removed? With FALSE (default) any NA will
#'        return NA.
#' @param check_geos A logical. If TRUE (default) gives an error if \code{weight_df} does
#'        not have all countries of \code{geo}.
#' @param mean_type A meant type to calculate "geom" for geometric and "arit" for arithmetic.
#'
#' @return A weighted vector or vector of NA if any value in x is NA.
#'
#' @export
#' @examples
#'
#' x <- c(1, 2, 1)
#' geo <- c("FI", "DE", "SE")
#' w_df <- tibble(geo_base = c("FI", "FI", "FI", "DE", "DE", "DE", "SE", "SE", "SE"),
#'                geo =      c("FI", "DE", "SE", "FI", "DE", "SE", "FI", "DE", "SE"),
#'                time = 2015,
#'                weight =   c(NA, 0.5, 0.25, 1, NA, 1, 1, 1, NA))
#' weight_index(x, geo, 2015, weight_df = w_df, na.rm = TRUE, mean_type = "arit")
#' weight_index2(x, geo, 2015, geos = geo, weight_df = w_df, na.rm = TRUE)
#' weight_index(x, geo, 2015, weight_df = weights_ecb, na.rm = TRUE)
#' weight_index2(x, geo, 2015, geos = geo, weight_df = weights_ecb, na.rm = TRUE)
weight_index <- function(x, geo, time, weight_df,
                         nearest = TRUE, na_zero = TRUE, na.rm = FALSE,
                         check_geos = TRUE,
                         mean_type = "geom") {

  if (lubridate::is.Date(time)) time <- lubridate::year(time)
  if (!all(time == mean(time))) stop("Time should be unique")
  time <- time[1]

  if (any(is.na(x))){
    if (!na.rm) {
      return(rlang::rep_along(x, NA))
    } else {
      warning("Index missing in ", time, " for: ", paste0(geo[is.na(x)], collapse = ", "))
    }
  }

  if (nearest) time <- weight_df$time[which.min(abs(weight_df$time-time))]
  if (check_geos) {
    in_base <- geo %in% weight_df$geo_base
    if (any(!in_base)) stop("Missing geo(s) in weight_df geo_base: ", paste0(geo[!in_base], collapse = ", "))

    in_geo <- geo %in% weight_df$geo
    if (any(!in_geo)) stop("Missing geo(s) in weight_df geo: ", paste0(geo[!in_geo], collapse = ", "))
  }

  w_df <- weight_df[weight_df$time == time, ]

  # w <- weight_df[weight_df$time == time & weight_df$geo %in% geo & weight_df$geo_base %in% geo,]
  # w <- w[order(match(w$geo, geo)),]
  if (na_zero) w_df$weight[is.na(w_df$weight)] <- 0

  weighted_other <- purrr::map_dbl(geo, ~ weight_function(.x, x, geo, w_df,
                                                          check_geos = check_geos,
                                                          mean_type = mean_type))
  y <- 100 * x / weighted_other

  y
}

#' @describeIn weight_index A function to use subset of observations (geos).
#' @export

weight_index2 <- function(x, geo, time, geos, weight_df,
                          nearest = TRUE, na_zero = TRUE, na.rm = FALSE,
                          mean_type = "geom") {

  # xx <<- x
  # geogeo <<- geo
  # timetime <<- time
  # geosgeos <<- geos
  # weight_df__ <<-weight_df

  # Check time
  if (lubridate::is.Date(time)) time <- lubridate::year(time)
  if (!all(time == mean(time))) stop("Time should be unique")
  time <- time[1]

  # check geos in weight_df and stop if missing
  geos_in_df <-
    weight_df %>%
    distinct(geo, geo_base) |>
    map_at(c("geo", "geo_base"), ~(geos %in% .x)) |>
    as_tibble() %>%
    apply(1, all)
  if (!all(geos_in_df)) stop(paste0(geos[!geos_in_df], collapse = " "), " are not present in weight_df")

  # check geos in geo and return NA if any missing
  geos_in_geo <- geos %in% geo
  if (!all(geos_in_geo)) {
    warning(paste0(geos[!geos_in_geo], collapse = " "), " are not present in geo at year ", time,
            ". NA is returned")
    return(x[NA])
  }

  # Values to weigth over based on geos
  geo <- as.character(geo)
  sel_obs <- geo %in% geos
  z <- x[sel_obs]
  z_geo <- geo[sel_obs]

  # If missing in selected return NA
  missing_geos <- is.na(z)
  if (any(missing_geos)){
    warning(paste0(z_geo[missing_geos], collapse = " "), " are missing at year ", time,
            ". NA is returned")
    return(x[NA])
  }

  if (nearest) time <- weight_df$time[which.min(abs(weight_df$time-time))]

  w_df <- weight_df[weight_df$time == time, ]


  if (na_zero) w_df$weight[is.na(w_df$weight)] <- 0

  weighted_other <- purrr::map_dbl(z_geo, ~ weight_function(.x, z, z_geo, w_df,
                                                            check_geos = FALSE,
                                                            mean_type = mean_type))
  y <- x[NA]
  y[sel_obs] <- 100 * z / weighted_other
  y

}




#' Weight function for weight_index
#'
#' @param one_geo A character of base geo
#' @param x A variable to weight
#' @param geo A vector of ids to weights
#' @param w_df A weigth data.frame
#' @param check_geos A locigal to check if all geos are in w_df
#' @param mean_type A meant type to calculate "geom" for geometric and "arit" for arithmetic

weight_function <- function(one_geo, x, geo, w_df, check_geos, mean_type){

  w_df2 <- w_df[w_df$geo_base == one_geo, ]
  # w <- w_df2[match(geo, w_df2$geo),]
  w <- suppressWarnings(left_join(tibble(geo = geo), w_df2, by = "geo"))

  # check for weights (must run before NAs are zeroed, otherwise missing
  # weights are silently treated as 0 and this check never fires)
  if (check_geos){
    if (any(is.na(w$weight) & w$geo != one_geo)) stop("Weights missing in ", one_geo, " for: ",
                                                      paste0(geo[is.na(w$weight) & w$geo != one_geo],
                                                             collapse = ", "))
  }

  # If own weight missing also value is set to missing (must also run
  # before NAs are zeroed below)
  if (is.na(w$weight[w$geo == one_geo])) x[w$geo == one_geo] <- NA

  w$weight[is.na(w$weight)] <- 0

  if (mean_type == "geom"){
    y <- weighted_gmean(x, w$weight, na.rm = TRUE)
  } else if (mean_type == "arit"){
    y <- weighted.mean(x, w$weight, na.rm = TRUE)
  } else {
    stop("A wrong mean type.")
  }
  y

}
