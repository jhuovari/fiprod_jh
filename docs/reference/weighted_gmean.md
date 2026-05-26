# Weighted geometric mean

Calculate weighted geometric mean

## Usage

``` r
weighted_gmean(x, w, na.rm = FALSE)
```

## Arguments

- x:

  a numeric vector

- w:

  a numeric vector for weights

- na.rm:

  A logical. Should missing x values be removed?

## Examples

``` r
x <- c(1,2,3, NA)
w <- c(0.25,0.5,0.25, NA)
weighted_gmean(x, w, na.rm = TRUE)
#> [1] 1.86121
```
