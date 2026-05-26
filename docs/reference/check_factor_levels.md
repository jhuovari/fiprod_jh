# Tabulate factor level combinations (by time) against `values`

Creates a complete cross-tabulation over all factor variables (excluding
those in `drop_vars`) and all unique time points, and reports the number
of rows and whether any non-missing numeric values exist in the `values`
column for each combination.

## Usage

``` r
check_factor_levels(df, drop_vars = character())
```

## Arguments

- df:

  A `data.frame` containing:

  - one column named `time` of class `Date`, and

  - one numeric column named `values`, and

  - one or more factor columns (dimensions).

- drop_vars:

  Character vector of factor column names to exclude from the
  cross-tabulation. Defaults to none.

## Value

A `data.frame` with one row per combination of factor levels and `time`,
plus `n` and `has_numeric` columns. Factor columns in the output retain
the original levels and ordering.

## Examples

``` r
df <- data.frame(
  country = factor(c("FI","FI","SE","SE","NO")),
  sector  = factor(c("A","B","A","B","A")),
  time    = as.Date(c("2020-01-01","2020-01-01","2020-01-01","2021-01-01","2021-01-01")),
  values  = c(1, NA, NA, 3, NA)
)
check_factor_levels(df)
#> # A tibble: 6 × 4
#>   country sector     n has_numeric
#>   <chr>   <chr>  <int> <lgl>      
#> 1 FI      A          1 TRUE       
#> 2 FI      B          1 FALSE      
#> 3 NO      A          1 FALSE      
#> 4 NO      B          1 FALSE      
#> 5 SE      A          1 FALSE      
#> 6 SE      B          1 TRUE       

# Excluding a factor from the grid:
check_factor_levels(df, drop_vars = "sector")
#> # A tibble: 3 × 3
#>   country     n has_numeric
#>   <chr>   <int> <lgl>      
#> 1 FI          2 TRUE       
#> 2 NO          2 FALSE      
#> 3 SE          2 TRUE       
```
