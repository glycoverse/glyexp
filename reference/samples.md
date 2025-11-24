# Get Samples or Variables of an Experiment

Getting the names of samples or variables of an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).
Syntax sugar for `colnames(exp$expr_mat)` and `rownames(exp$expr_mat)`.

## Usage

``` r
samples(exp)

variables(exp)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

## Value

A character vector of sample or variable names.

## Examples

``` r
exp <- toy_experiment
samples(exp)
#> [1] "S1" "S2" "S3" "S4" "S5" "S6"
variables(exp)
#> [1] "V1" "V2" "V3" "V4"
```
