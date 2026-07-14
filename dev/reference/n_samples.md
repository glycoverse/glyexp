# Get number of samples or variables of an experiment

Getting the number of samples or variables of an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md).
Syntax sugar for `ncol(exp$expr_mat)` and `nrow(exp$expr_mat)`.

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
as the default data container.

## Usage

``` r
n_samples(exp)

n_variables(exp)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md).

## Value

An integer with the number of samples or variables.
