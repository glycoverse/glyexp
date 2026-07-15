# Rename columns in the sample or variable information tibble

These two functions provide a way to rename columns in the sample or
variable information of an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
or `SummarizedExperiment`.

The same syntax as
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
is used. For example, to rename the "group" column in the sample
information tibble to "condition", use
`rename_col(exp, condition = group)`. Note that you can't rename the
"sample" column in the sample information tibble, as well as the
"variable" column in the variable information tibble. These two columns
are used to link the sample or variable information tibble to the
expression matrix.

## Usage

``` r
rename_col(exp, ...)

rename_row(exp, ...)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  or `SummarizedExperiment` object.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name pairs to rename. Use `new_name = old_name` to rename columns.

## Value

An object of the same class as `exp`.

## Identifier columns

For an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object, `sample` is a physical column in `sample_info`, and `variable`
is a physical column in `var_info`.

For a `SummarizedExperiment`, sample and variable identifiers live in
`colnames(exp)` and `rownames(exp)`, rather than in
[`SummarizedExperiment::colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
or
[`SummarizedExperiment::rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).
Observation verbs expose `colnames(exp)` as a virtual `.sample` column,
and variable verbs expose `rownames(exp)` as a virtual `.variable`
column. These dot-prefixed names distinguish dimension identifiers from
regular metadata columns. After the operation, the virtual column is
removed and its values are written back to the corresponding dimension
names.

Consequently, `sample` in `colData(exp)` and `variable` in
`rowData(exp)` remain ordinary metadata columns. The names `.sample` and
`.variable` are reserved; an input containing either name in the
corresponding metadata raises an error rather than overwriting that
column.

## Examples

``` r
toy_exp <- real_experiment
toy_exp
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>

# Rename columns in sample information tibble
rename_col(toy_exp, condition = group)
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>
#> ℹ Column data fields: condition <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>

# Rename columns in variable information tibble
rename_row(toy_exp, composition = glycan_composition)
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, composition <comp>, glycan_structure <struct>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
```
