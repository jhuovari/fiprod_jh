# Save a dataset to inst/extdata as Parquet (for package build time)

Writes a data frame to the package's `inst/extdata/` directory so that,
after installation, the file will be available under
`system.file("extdata", ...)`. Intended to be used during development
(not at runtime after installation).

## Usage

``` r
save_dat(
  x,
  filename = deparse(substitute(x)),
  dir = "inst/extdata",
  overwrite = FALSE
)
```

## Arguments

- x:

  A `data.frame` (or tibble) to be saved.

- filename:

  File name to write. Defaults to the name of `x` with a `.parquet`
  extension.

- dir:

  Directory where to write during development. Defaults to
  `"inst/extdata"`. The directory is created if it does not exist.

- overwrite:

  Logical, overwrite an existing file (default `FALSE`).

## Value

(Invisibly) the path to the written file.

## Examples

``` r
if (FALSE) { # \dontrun{
# During development of the package:
save_dat(mtcars) # writes inst/extdata/mtcars.parquet
} # }
```
