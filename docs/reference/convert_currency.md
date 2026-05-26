# Convert national currency values into another currency (with optional baseline date)

Converts values expressed in national currency (identified by `geo`)
into a target currency using an EUR-anchored exchange rate table.
Optionally, a `base_time` can be provided so that all conversions use
exchange rates from that single date.

## Usage

``` r
convert_currency(
  values,
  geo,
  time,
  to,
  exch = exh_eur_a,
  euro_geos = c("EA", "EA19", "EA20"),
  base_time = NULL,
  warn_if_missing = TRUE
)
```

## Arguments

- values:

  Numeric vector of values in national currency.

- geo:

  Character or factor vector of country/area codes (same length as
  values).

- time:

  Date vector of observation dates (same length as values).

- to:

  Target currency code (e.g. "USD").

- exch:

  Exchange rate table, defaults to `exh_eur_a`. Must contain columns:
  `time` (Date), `geo` (chr), `currency` (chr), `values` (numeric),
  where `values` is the rate "1 EUR = values units of currency".

- euro_geos:

  Character vector of geo codes that are already in EUR (default:
  c("EA","EA19","EA20")). For these, the EUR-\>nat factor is 1.

- base_time:

  Optional baseline date (Date or coercible). If supplied, all
  conversions use exchange rates from this date for both EUR-\>nat and
  EUR-\>to. If `NULL` (default), rates are taken at each row's `time`.

- warn_if_missing:

  Logical; warn when required rates are missing. Default TRUE.

## Value

Numeric vector of converted values (same length as `values`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Per-row dates:
df <- dplyr::mutate(df,
  values_usd = convert_currency(values, geo, time, to = "USD")
)

# Using baseline date (e.g., convert everything using 2020-01-01 rates):
df <- dplyr::mutate(df,
  values_usd_2020 = convert_currency(values, geo, time, to = "USD",
                                     base_time = as.Date("2020-01-01"))
)
} # }
```
