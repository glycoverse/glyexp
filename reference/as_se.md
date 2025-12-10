# Convert experiment to SummarizedExperiment

Convert an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object to a `SummarizedExperiment` object. This function maps the
experiment structure to SummarizedExperiment format:

- `expr_mat` becomes the main assay

- `sample_info` becomes `colData`

- `var_info` becomes `rowData`

- `meta_data` becomes `metadata`

## Usage

``` r
as_se(exp, assay_name = "counts")
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  object to convert.

- assay_name:

  Character string specifying the name for the assay. Default is
  "counts".

## Value

A `SummarizedExperiment` object.

## Examples

``` r
# Convert toy experiment to SummarizedExperiment
se <- as_se(toy_experiment)
#> Warning: replacing previous import ‘S4Arrays::makeNindexFromArrayViewport’ by ‘DelayedArray::makeNindexFromArrayViewport’ when loading ‘SummarizedExperiment’
se
#> class: SummarizedExperiment 
#> dim: 4 6 
#> metadata(2): exp_type glycan_type
#> assays(1): counts
#> rownames(4): V1 V2 V3 V4
#> rowData names(3): protein peptide glycan_composition
#> colnames(6): S1 S2 ... S5 S6
#> colData names(2): group batch
```
