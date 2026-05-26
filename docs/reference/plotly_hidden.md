# Customize Plotly chart: keep only download button and preselect geos

This helper modifies a Plotly object (typically created with
[`ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html)) so that:

- Only the "Download as PNG" button is shown in the modebar.

- The Plotly logo is hidden.

- Only the geos listed in `keep_geos` are visible at start; all other
  traces are set to `visible = "legendonly"`.

## Usage

``` r
plotly_hidden(p, keep_geos)
```

## Arguments

- p:

  A Plotly object.

- keep_geos:

  Character vector of geo codes to be visible initially.

## Value

A modified Plotly object with customized modebar and visibility.

## Examples

``` r
if (FALSE) { # \dontrun{
pp <- ggplot2::ggplot(dat, ggplot2::aes(time, values, colour = geo)) +
  ggplot2::geom_line()
p <- plotly::ggplotly(pp)
p2 <- plotly_hidden(p, keep_geos = c("FI","SE","EA20","US"))
p2
} # }
```
