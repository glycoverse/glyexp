# Split an experiment

Devides an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
into a list of
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects by `f`.

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.md)
as the default data container.

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
  A column in `var_info` or `sample_info` that `as.factor(f)` defines
  the grouping.

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
