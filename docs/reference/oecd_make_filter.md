# Create an OECD Filter String

This function takes a list of filtering arguments and constructs a
filter string for an OECD API call.

## Usage

``` r
oecd_make_filter(x)
```

## Arguments

- x:

  A list of strings representing different filter criteria for the OECD
  API.

## Value

A single string with the filter criteria concatenated using '+' and '.'
as separators.

## Examples

``` r
make_oecd_filter(list("A", "sna_geo", "", "", "sna6a_transact", "", "sna_activity", "", "", "sna_measures", "", ""))
#> Error in make_oecd_filter(list("A", "sna_geo", "", "", "sna6a_transact",     "", "sna_activity", "", "", "sna_measures", "", "")): could not find function "make_oecd_filter"
```
