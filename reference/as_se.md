# Convert experiment to SummarizedExperiment

Convert an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object to a `SummarizedExperiment` object. This function maps the
experiment structure to SummarizedExperiment format:

- `expr_mat` becomes the main assay

- `sample_info` becomes `colData`

- `var_info` becomes `rowData`

- `meta_data` becomes `metadata`

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.md)
as the default data container.

## Usage

``` r
as_se(exp, assay_name = "abundance")
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  object to convert.

- assay_name:

  Character string specifying the name for the assay. Default is
  "abundance".

## Value

A `SummarizedExperiment` object.
