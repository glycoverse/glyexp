# Set the meta data of an experiment

Set meta data values for the experiment, like the experiment type
("glycomics" or "glycoproteomics"), or the glycan type ("N", "O",
"O-GalNAc", "O-Man", "O-Fuc", "O-GlcNAc", "O-Glc", "HMO", "GSL", "GAG",
or "GPI").

## Usage

``` r
set_meta_data(exp, x, value)

set_exp_type(exp, value)

set_glycan_type(exp, value)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- x:

  A string, the name of the meta data field.

- value:

  The value to set for the meta data field.

## Value

The modified
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object.
