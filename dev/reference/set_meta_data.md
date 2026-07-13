# Set the meta data of an experiment

Set meta data values for the experiment, like the experiment type
("glycomics" or "glycoproteomics"), or the glycan type ("N", "O",
"O-GalNAc", "O-Man", "O-Fuc", "O-GlcNAc", "O-Glc", "HMO", "GSL", "GAG",
or "GPI").

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
as the default data container.

## Usage

``` r
set_meta_data(exp, x, value)

set_exp_type(exp, value)

set_glycan_type(exp, value)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md).

- x:

  A string, the name of the meta data field.

- value:

  The value to set for the meta data field.

## Value

The modified
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object.
