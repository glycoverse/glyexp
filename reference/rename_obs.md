# Rename columns in the sample or variable information tibble

These two functions provide a way to rename columns in the sample or
variable information tibble of an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

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
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name pairs to rename. Use `new_name = old_name` to rename columns.

## Value

An new
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object.

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
