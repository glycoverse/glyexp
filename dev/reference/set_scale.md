# Set the abundance scale of an experiment

Update scale metadata without transforming abundance values. The
resulting object is validated, so `"raw"` requires non-negative
abundance values.

## Usage

``` r
set_scale(exp, value)
```

## Arguments

- exp:

  A
  [`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
  or
  [`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
  object.

- value:

  A scale: `"raw"`, `"log"` (natural logarithm), `"log2"`, `"log10"`, or
  `"logx"` (a transformed logarithmic or log-like scale).

## Value

The modified object, with the same class as `exp`.

## See also

[`get_scale()`](https://glycoverse.github.io/glyexp/dev/reference/get_scale.md)

## Examples

``` r
exp <- set_scale(real_experiment2, "raw")
get_scale(exp)
#> [1] "raw"
```
