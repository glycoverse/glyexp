# Get the sample information of an experiment

A `tibble` of sample information, with the first column being "sample".

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.md)
as the default data container.

## Usage

``` r
get_sample_info(exp)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

## Value

A tibble of sample information.
