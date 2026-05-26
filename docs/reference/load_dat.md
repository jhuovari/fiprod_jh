# Load a dataset from extdata (installed package) as Parquet

Reads a Parquet file stored under the package's `inst/extdata/` at build
time. At runtime, the function first tries `system.file("extdata", ...)`
(works for installed packages and with
[`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)),
and if not found, falls back to the development path `inst/extdata/`
under the package root (found via `DESCRIPTION`).

## Usage

``` r
load_dat(filename, package = NULL, must_work = TRUE)
```

## Arguments

- filename:

  File name to read (with or without `.parquet`).

- package:

  Package name where the file resides. Defaults to `NULL`, in which case
  the function tries to infer the package name; if that fails, it will
  still try the development path under `inst/extdata/`.

- must_work:

  If `TRUE` (default), error if the file cannot be located.

## Value

A `data.frame` loaded from the Parquet file.

## Examples

``` r
if (FALSE) { # \dontrun{
# After installation or devtools::load_all():
df <- load_dat("mtcars.parquet")

# During development when not using load_all(), the function can still
# locate inst/extdata/ by walking up to DESCRIPTION:
df <- load_dat("mtcars.parquet")
} # }
```
