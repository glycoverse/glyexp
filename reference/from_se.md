# Convert SummarizedExperiment to experiment

Convert a `SummarizedExperiment` object to an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object. This function maps the SummarizedExperiment structure to
experiment format:

- The main assay becomes `expr_mat`

- `colData` becomes `sample_info`

- `rowData` becomes `var_info`

- `metadata` becomes `meta_data`

## Usage

``` r
from_se(se, assay_name = NULL, exp_type = NULL, glycan_type = NULL)
```

## Arguments

- se:

  A `SummarizedExperiment` object to convert.

- assay_name:

  Character string specifying which assay to use. If NULL (default),
  uses the first assay.

- exp_type:

  Character string specifying experiment type. Must be either
  "glycomics", "glycoproteomics", or "others". If NULL, will try to
  extract from metadata, otherwise defaults to "glycomics".

- glycan_type:

  Character string specifying glycan type. Must be either "N",
  "O-GalNAc", "O-GlcNAc", "O-Man", "O-Fuc", or "O-Glc". If NULL, will
  try to extract from metadata, otherwise defaults to "N".

## Value

An
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object.

## Examples

``` r
# Convert SummarizedExperiment back to experiment
se <- as_se(toy_experiment)
exp_back <- from_se(se, exp_type = "glycomics", glycan_type = "N")
exp_back
#> 
#> ── Glycomics Experiment ────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
```
