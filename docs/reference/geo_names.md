# Retrieve country/region name vectors by language

Returns a named character vector mapping geo codes to country or region
names in the selected language.

## Usage

``` r
geo_names(lang = c("fi", "en"))
```

## Arguments

- lang:

  Language code: "fi" (Finnish) or "en" (English). Case-insensitive.

## Value

Named character vector with geo code names and country labels.

## Examples

``` r
geo_names("fi")
#>              PT              SE              NO              ES            EA20 
#>     "Portugali"        "Ruotsi"         "Norja"       "Espanja" "Euroalue (20)" 
#>              US              AT              BE              IT              DE 
#>   "Yhdysvallat"      "Itävalta"        "Belgia"        "Italia"         "Saksa" 
#>              NL              JP              FI              DK              FR 
#>    "Alankomaat"        "Japani"         "Suomi"        "Tanska"        "Ranska" 
geo_names("en")
#>               PT               SE               NO               ES 
#>       "Portugal"         "Sweden"         "Norway"          "Spain" 
#>             EA20               US               AT               BE 
#> "Euro area (20)"  "United States"        "Austria"        "Belgium" 
#>               IT               DE               NL               JP 
#>          "Italy"        "Germany"    "Netherlands"          "Japan" 
#>               FI               DK               FR 
#>        "Finland"        "Denmark"         "France" 
df = data.frame(geo = c("FI", "SE"))
dplyr::mutate(df, geo_name = dplyr::recode(geo, !!!geo_names("en")))
#>   geo geo_name
#> 1  FI  Finland
#> 2  SE   Sweden
```
