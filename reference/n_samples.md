# Get number of samples or variables of an experiment

Getting the number of samples or variables of an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).
Syntax sugar for `ncol(exp$expr_mat)` and `nrow(exp$expr_mat)`.

## Usage

``` r
n_samples(exp)

n_variables(exp)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

## Value

An integer with the number of samples or variables.

## Examples

``` r
exp <- toy_experiment
n_samples(exp)
#> [1] 6
n_variables(exp)
#> [1] 4
```
