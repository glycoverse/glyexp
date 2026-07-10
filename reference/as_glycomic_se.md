# Coerce to GlycomicSE

`as_glycomic_se()` converts supported objects to a `GlycomicSE` object.
Existing `GlycomicSE` objects are returned unchanged.
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects are first converted with
[`as_se()`](https://glycoverse.github.io/glyexp/reference/as_se.md), and
`SummarizedExperiment` objects are reclassified after `GlycomicSE`
validity checks pass.

`is_glycomic_se()` checks whether an object inherits from `GlycomicSE`.

## Usage

``` r
as_glycomic_se(x)

is_glycomic_se(x)
```

## Arguments

- x:

  An object to coerce or check.

## Value

`as_glycomic_se()` returns a `GlycomicSE` object. `is_glycomic_se()`
returns a logical value.
