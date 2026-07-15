# Get the expression matrix of an experiment

A `matrix` of expression values with samples as columns and variables as
rows.

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.md)
as the default data container.

## Usage

``` r
get_expr_mat(exp)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

## Value

A matrix of expression values.
