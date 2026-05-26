# Relative to peer geometric mean (one column, tidy-eval)

For each base observation (time × geo × variable slice), computes one
numeric column relative to the weighted geometric mean of its peers
(other countries) as defined by `weights`.

## Usage

``` r
relative_to_peer_geom_mean(
  dat,
  value_col,
  weights,
  by_vars = c("var_id", "measure", "unit_measure", "price_base"),
  out_col = NULL,
  mean_col = NULL
)
```

## Arguments

- dat:

  Data frame with at least columns `time` (Date), `geo`, the slice
  variables in `by_vars`, and the value column `value_col`.

- value_col:

  Unquoted column name in `dat` to use as value (e.g. `values`), must be
  numeric.

- weights:

  Weight table with columns: `geo_base`, `time` (numeric year), `geo`
  (peer), `weight`.

- by_vars:

  Character vector: slice variables that must match between base and
  peers. Default `c("var_id","measure","unit_measure","price_base")`.

- out_col:

  String: name of the output relative column. Defaults to
  `paste0("rel_", value_col_name)`.

- mean_col:

  String: name of the peer-mean column. Defaults to
  `paste0("geom_mean_peers_", value_col_name)`.

## Value

`dat` with two extra columns:

- `mean_col`: weighted geometric mean of peers

- `out_col`: value_col / mean_col

## Details

All peers with positive weight must have finite and strictly positive
values; otherwise the peer mean is set to NA for that base observation.

## Examples

``` r
# relative ULC:
# dat_rel <- relative_to_peer_geom_mean(
#   dat_oecd_pdb_ulc,
#   values,
#   weights_ecfin20,
#   out_col = "ulc_rel"
# )
```
