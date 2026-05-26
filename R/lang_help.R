#' Retrieve country/region name vectors by language
#'
#' Returns a named character vector mapping geo codes
#' to country or region names in the selected language.
#'
#' @param lang Language code: "fi" (Finnish) or "en" (English). Case-insensitive.
#'
#' @return Named character vector with geo code names and country labels.
#' @examples
#' geo_names("fi")
#' geo_names("en")
#' df = data.frame(geo = c("FI", "SE"))
#' dplyr::mutate(df, geo_name = dplyr::recode(geo, !!!geo_names("en")))
#'
#' @export
geo_names <- function(lang = c("fi", "en")) {
  lang <- tolower(lang[1])

  geo_names_list[[lang]]
}
