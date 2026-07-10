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
  "glycomics", "glycoproteomics", "traitomics", "traitproteomics", or
  "others". If not supplied, will try to extract from metadata. If
  unavailable there, an error is issued.

- glycan_type:

  Character string specifying glycan type. Must be one of the valid
  `glycan_type` values accepted by
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).
  If not supplied, will try to extract from metadata. If unavailable
  there, an error is issued unless `exp_type` is "others", where `NULL`
  is allowed.

## Value

An
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object.

## Examples

``` r
# Convert SummarizedExperiment back to experiment
se <- as_se(toy_experiment)
exp_back <- from_se(se, exp_type = "glycomics", glycan_type = "N")
#> Column group converted to <factor>.
#> Column batch converted to <factor>.
exp_back
#> 
#> ── Glycomics Experiment ────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: group <fct>, batch <fct>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
```
