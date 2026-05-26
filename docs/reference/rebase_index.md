# Rebase a numeric series to an index with a chosen base year

This function converts a numeric time series into an index, where the
level in the specified `baseyear` equals `basevalue`. The base-year
value is the mean of all observations in that year. Works in both base R
and inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

## Usage

``` r
rebase_index(x, time, baseyear, basevalue = 100)
```

## Arguments

- x:

  Numeric vector of values.

- time:

  A vector of dates or years corresponding to `x`.

- baseyear:

  Numeric year or vector of years to use as base.

- basevalue:

  Numeric index value assigned to the base period. If `NULL`, the base
  value becomes the mean of the base-year observations.

## Value

A numeric vector of rebased index values.

## Examples

``` r
df |> mutate(index = rebase_index(values, time, 2007))
#> Error in mutate(df, index = rebase_index(values, time, 2007)): could not find function "mutate"
df |> mutate(index = rebase_index(values, time, baseyear = c(2006, 2007)))
#> Error in mutate(df, index = rebase_index(values, time, baseyear = c(2006,     2007))): could not find function "mutate"
```
