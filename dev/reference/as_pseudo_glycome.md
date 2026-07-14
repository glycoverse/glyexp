# Convert a glycoproteomics experiment to a pseudo-glycome experiment

Transforms a glycoproteomics-type experiment into a glycomics-type
experiment by aggregating expression values by glycan structure (if
available) or glycan composition.

This function implements the "pseudo-glycome" method described in
[doi:10.1038/s41467-026-68579-x](https://doi.org/10.1038/s41467-026-68579-x)
, which aggregates glycoproteomic data by glycans to simulate glycome
data when real glycome is unavailable.

## Usage

``` r
as_pseudo_glycome(exp, aggr_method = c("sum", "mean", "median"))
```

## Arguments

- exp:

  A glycoproteomics
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
  or a
  [`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md).

- aggr_method:

  Aggregation method to use. One of "sum", "mean", or "median". Default
  is "sum". Note that glycopeptides can have different ionization
  efficiencies, so none of these methods are technically rigorous.

## Value

If `exp` is an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md),
a glycomics-type
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
with aggregated expression values.

If `exp` is a
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md),
a
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
with aggregated expression values.

The variable metadata will contain only `glycan_composition` and
`glycan_structure` (if present in input) columns.

## Details

**Aggregation behavior:**

- If `glycan_structure` column exists in `var_info`, aggregation is done
  by glycan structure (more specific)

- Otherwise, aggregation is done by `glycan_composition`

- Expression values are aggregated within each glycan group using the
  specified `aggr_method`

**Limitation:** Glycopeptides can have different ionization
efficiencies, so the aggregation operation is not technically rigorous
regardless of the method used. Use results with caution.

## Examples

``` r
library(glyrepr)
as_pseudo_glycome(real_experiment)
#> 
#> ── GlycomicSE ──────────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 968 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: glycan_structure <struct>, glycan_composition <comp>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
```
