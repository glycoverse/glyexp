# Coerce to GlycoproteomicSE

`as_glycoproteomic_se()` converts supported objects to a
`GlycoproteomicSE` object. Existing `GlycoproteomicSE` objects are
returned unchanged.
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
objects are first converted with
[`as_se()`](https://glycoverse.github.io/glyexp/dev/reference/as_se.md),
and `SummarizedExperiment` objects are reclassified after
`GlycoproteomicSE` validity checks pass.

`is_glycoproteomic_se()` checks whether an object inherits from
`GlycoproteomicSE`.

## Usage

``` r
as_glycoproteomic_se(x)

is_glycoproteomic_se(x)
```

## Arguments

- x:

  An object to coerce or check.

## Value

`as_glycoproteomic_se()` returns a `GlycoproteomicSE` object.
`is_glycoproteomic_se()` returns a logical value.
