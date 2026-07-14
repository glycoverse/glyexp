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
