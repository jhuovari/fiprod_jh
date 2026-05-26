# Calculate weighted index

Uses weights from a data frame.

## Usage

``` r
weight_index(
  x,
  geo,
  time,
  weight_df,
  nearest = TRUE,
  na_zero = TRUE,
  na.rm = FALSE,
  check_geos = TRUE,
  mean_type = "geom"
)

weight_index2(
  x,
  geo,
  time,
  geos,
  weight_df,
  nearest = TRUE,
  na_zero = TRUE,
  na.rm = FALSE,
  mean_type = "geom"
)
```

## Arguments

- x:

  A vector to weight.

- geo:

  A vector to indicate countries.

- time:

  A year (or date which is converted to a year) for weights.

- weight_df:

  A weigthing data.frame in long form. Should have time, geo_base and
  geo columns.

- nearest:

  A logical whether to use nearest year for weight table.

- na.rm:

  A logical. Should missing values be removed? With FALSE (default) any
  NA will return NA.

- check_geos:

  A logical. If TRUE (default) gives an error if `weight_df` does not
  have all countries of `geo`.

- mean_type:

  A meant type to calculate "geom" for geometric and "arit" for
  arithmetic.

- geos:

  A string vector of geos to weight over.

## Value

A weighted vector or vector of NA if any value in x is NA.

## Details

weight_index2 allowes spesifing subset of observatios (geos) for
weighting.

## Functions

- `weight_index2()`: A function to use subset of observations (geos).

## Examples

``` r
x <- c(1, 2, 1)
geo <- c("FI", "DE", "SE")
w_df <- tibble(geo_base = c("FI", "FI", "FI", "DE", "DE", "DE", "SE", "SE", "SE"),
               geo =      c("FI", "DE", "SE", "FI", "DE", "SE", "FI", "DE", "SE"),
               time = 2015,
               weight =   c(NA, 0.5, 0.25, 1, NA, 1, 1, 1, NA))
#> Error in tibble(geo_base = c("FI", "FI", "FI", "DE", "DE", "DE", "SE",     "SE", "SE"), geo = c("FI", "DE", "SE", "FI", "DE", "SE",     "FI", "DE", "SE"), time = 2015, weight = c(NA, 0.5, 0.25,     1, NA, 1, 1, 1, NA)): could not find function "tibble"
weight_index(x, geo, 2015, weight_df = w_df, na.rm = TRUE, mean_type = "arit")
#> Error: object 'w_df' not found
weight_index2(x, geo, 2015, geos = geo, weight_df = w_df, na.rm = TRUE)
#> Error in map_at(weight_df %>% distinct(geo, geo_base), c("geo", "geo_base"),     ~(geos %in% .x)): could not find function "map_at"
weight_index(x, geo, 2015, weight_df = weights_ecb, na.rm = TRUE)
#> Error: object 'weights_ecb' not found
weight_index2(x, geo, 2015, geos = geo, weight_df = weights_ecb, na.rm = TRUE)
#> Error in map_at(weight_df %>% distinct(geo, geo_base), c("geo", "geo_base"),     ~(geos %in% .x)): could not find function "map_at"
```
