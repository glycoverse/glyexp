# Dimname for experiment

The dimnames method for
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
objects are the dimnames of their expression matrix.

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
as the default data container.

## Usage

``` r
# S3 method for class 'glyexp_experiment'
dimnames(x, ...)
```

## Arguments

- x:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md).

- ...:

  Ignored.

## Value

A list with the dimnames of the expression matrix.
