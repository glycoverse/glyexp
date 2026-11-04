# Get the abundance scale of an experiment

Extract the `scale` field from the metadata of a
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.md)
object without inferring or transforming abundance values.

## Usage

``` r
get_scale(exp)
```

## Arguments

- exp:

  A
  [`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
  or
  [`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.md)
  object.

## Value

A character string giving the abundance scale: `"raw"`, `"log"` (natural
logarithm), `"log2"`, `"log10"`, or `"logx"` (a transformed logarithmic
or log-like scale). Returns `NULL` if the metadata field is absent.

## See also

[`S4Vectors::metadata()`](https://rdrr.io/pkg/S4Vectors/man/Annotated-class.html),
[`get_glycan_type()`](https://glycoverse.github.io/glyexp/reference/get_glycan_type.md)

## Examples

``` r
get_scale(real_experiment)
#> [1] "raw"
get_scale(real_experiment2)
#> [1] "raw"
```
