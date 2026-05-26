# Mass weighting

Weight all variables except.

## Usage

``` r
weight_all(.data, geo, time, except, weight_df)

weight_at(.data, geo, time, at, weight_df)
```

## Arguments

- .data:

  A tbl.

- geo:

  A vector to indicate countries.

- time:

  A year (or date which is converted to a year) for weights.

- except:

  Variables to exclude, a character vector.

- ar:

  Variables to include, a character vector.

- weitht_df:

  A weigthing data.frame in long form. Should have time, geo_base and
  geo columns.

## Functions

- `weight_at()`: To spesify variables to weight
