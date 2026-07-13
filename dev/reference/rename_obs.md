# Rename columns in the sample or variable information tibble

These two functions provide a way to rename columns in the sample or
variable information of an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
or `SummarizedExperiment`.

The same syntax as
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
is used. For example, to rename the "group" column in the sample
information tibble to "condition", use
`rename_obs(exp, condition = group)`. Note that you can't rename the
"sample" column in the sample information tibble, as well as the
"variable" column in the variable information tibble. These two columns
are used to link the sample or variable information tibble to the
expression matrix.

## Usage

``` r
rename_obs(exp, ...)

rename_var(exp, ...)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
  or `SummarizedExperiment` object.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name pairs to rename. Use `new_name = old_name` to rename columns.

## Value

An object of the same class as `exp`.

## Identifier columns

For an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
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
toy_exp <- toy_experiment
toy_exp
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>

# Rename columns in sample information tibble
rename_obs(toy_exp, condition = group)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: condition <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>

# Rename columns in variable information tibble
rename_var(toy_exp, composition = glycan_composition)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, composition <chr>
```
