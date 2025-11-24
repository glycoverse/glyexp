# Split an experiment

Devides an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
into a list of
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects by `f`.

## Usage

``` r
# S3 method for class 'glyexp_experiment'
split(x, f, drop = FALSE, where = "var_info", ...)
```

## Arguments

- x:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- f:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  A column in `var_info` or `sample_info` that `as.factor(f)`defines the
  grouping.

- drop:

  Logical indicating if levels that do not occur should be dropped.
  Defaults to FALSE.

- where:

  Where to find the column, "var_info" or "sample_info".

- ...:

  Ignored

## Value

A named list of
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects.

## Examples

``` r
split(toy_experiment, group, where = "sample_info")
#> $A
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
#> 
#> $B
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
#> 
```
