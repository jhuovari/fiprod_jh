# Clean and reshape OECD SDMX data

This function parses dates, converts character columns to factors,
selects and renames specified variables, creates a `values` column from
`obsValues` (or `obsValue`), and drops all other columns. It warns if
any dropped factor variables have more than one level.

## Usage

``` r
oecd_clean_data(
  df = NULL,
  sdmx_obj = NULL,
  drop_vars = NULL,
  vars = c(),
  freq = "A"
)
```

## Arguments

- df:

  A data.frame containing OECD SDMX data.

- vars:

  A named character vector specifying variables to keep and rename, in
  the form `c(new_name = "old_name")`. The old names must exist in `df`.

- freq:

  Data frequency: `"A"` for annual data, `"Q"` for quarterly data.
  Determines the date parsing format for the `obsTime` column.

## Value

A tibble with renamed variables from `vars` and a `values` column.

## Examples

``` r
if (FALSE) { # \dontrun{
vars <- c(geo = "LOCATION", measure = "MEASURE")
cleaned <- oecd_clean_data(smdx_data, vars = vars, freq = "A")
} # }
```
