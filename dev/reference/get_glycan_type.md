# Get the glycan type of an experiment

Extract the `glycan_type` field from the metadata of a
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
object.

## Usage

``` r
get_glycan_type(exp)
```

## Arguments

- exp:

  A
  [`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
  or
  [`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
  object. Legacy
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
  objects are also supported with a deprecation warning.

## Value

A character string giving the glycan type, or `NULL` if the metadata
field is absent.

## See also

[`S4Vectors::metadata()`](https://rdrr.io/pkg/S4Vectors/man/Annotated-class.html)

## Examples

``` r
get_glycan_type(real_experiment)
#> [1] "N"
get_glycan_type(real_experiment2)
#> [1] "N"
```
