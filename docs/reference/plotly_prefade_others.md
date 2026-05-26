# Prefade non-selected geos in a Plotly chart and toggle color/legend on click

Takes a Plotly object (e.g., from
[`ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html)), keeps
traces listed in `keep_geos` with their original colors and visible in
the legend, and renders all other traces initially in a light gray color
(hidden from the legend). Clicking a gray line toggles it to its
original color and adds it to the legend; clicking again fades it back
and hides it.

## Usage

``` r
plotly_prefade_others(
  p,
  keep_geos,
  gray_color = "#d3d3d3",
  keep_download_only = TRUE
)
```

## Arguments

- p:

  A Plotly object created e.g. by
  [`ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html).

- keep_geos:

  Character vector of geo codes that should start colored and visible in
  the legend.

- gray_color:

  Hex color used for faded traces (default `"#d3d3d3"`).

- keep_download_only:

  Logical; if `TRUE` (default) keep only the "Download as PNG" button
  and hide the Plotly logo.

## Value

A modified Plotly object with custom initial styling and an interactive
click handler to toggle trace color and legend visibility.

## Details

Trace name detection is based on `trace$name` or `trace$legendgroup`
after stripping common prefixes (e.g. `"geo="`) and anything after a
comma.

## Examples

``` r
if (FALSE) { # \dontrun{
pp <- ggplot2::ggplot(dat, ggplot2::aes(time, values, colour = geo)) +
  ggplot2::geom_line()
p  <- plotly::ggplotly(pp)
p2 <- plotly_prefade_others(p, keep_geos = c("FI","SE","EA20","US"))
p2
} # }
```
