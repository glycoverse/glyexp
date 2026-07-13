# Convert experiment to SummarizedExperiment

Convert an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object to a `SummarizedExperiment` object. This function maps the
experiment structure to SummarizedExperiment format:

- `expr_mat` becomes the main assay

- `sample_info` becomes `colData`

- `var_info` becomes `rowData`

- `meta_data` becomes `metadata`

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
as the default data container.

## Usage

``` r
as_se(exp, assay_name = "abundance")
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
  object to convert.

- assay_name:

  Character string specifying the name for the assay. Default is
  "abundance".

## Value

A `SummarizedExperiment` object.

## Examples

``` r
# Convert toy experiment to SummarizedExperiment
se <- as_se(toy_experiment)
#> Warning: `as_se()` was deprecated in glyexp 0.16.0.
#> ℹ Use `GlycomicSE()` or `GlycoproteomicSE()` as the default data container.
se
#> class: SummarizedExperiment 
#> dim: 4 6 
#> metadata(2): exp_type glycan_type
#> assays(1): abundance
#> rownames(4): V1 V2 V3 V4
#> rowData names(3): protein peptide glycan_composition
#> colnames(6): S1 S2 ... S5 S6
#> colData names(2): group batch
```
