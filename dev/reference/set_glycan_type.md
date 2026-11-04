# Set the glycan type of an experiment

Update glycan type metadata and validate the resulting object. Glycan
structures and compositions are not changed.

## Usage

``` r
set_glycan_type(exp, value)
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

- value:

  A glycan type: `"N"`, `"O"`, `"O-GalNAc"`, `"O-Man"`, `"O-Fuc"`,
  `"O-GlcNAc"`, `"O-Glc"`, `"HMO"`, `"GSL"`, `"GAG"`, `"GPI"`, or
  `"others"`.

## Value

The modified object, with the same class as `exp`.

## See also

[`get_glycan_type()`](https://glycoverse.github.io/glyexp/dev/reference/get_glycan_type.md)

## Examples

``` r
exp <- set_scale(real_experiment2, "raw")
exp <- set_glycan_type(exp, "N")
get_glycan_type(exp)
#> [1] "N"
```
