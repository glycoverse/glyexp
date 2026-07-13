# Get Samples or Variables of an Experiment

Getting the names of samples or variables of an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md).
Syntax sugar for `colnames(exp$expr_mat)` and `rownames(exp$expr_mat)`.

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
as the default data container.

## Usage

``` r
samples(exp)

variables(exp)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md).

## Value

A character vector of sample or variable names.

## Examples

``` r
exp <- toy_experiment
samples(exp)
#> Warning: `samples()` was deprecated in glyexp 0.16.0.
#> ℹ Use `GlycomicSE()` or `GlycoproteomicSE()` as the default data container.
#> [1] "S1" "S2" "S3" "S4" "S5" "S6"
variables(exp)
#> Warning: `variables()` was deprecated in glyexp 0.16.0.
#> ℹ Use `GlycomicSE()` or `GlycoproteomicSE()` as the default data container.
#> [1] "V1" "V2" "V3" "V4"
```
