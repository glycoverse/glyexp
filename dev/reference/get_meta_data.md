# Get the meta data of an experiment

Meta data is some descriptions about the experiment, like the experiment
type ("glycomics" or "glycoproteomics"), or the glycan type ("N", "O",
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
get_meta_data(exp, x = NULL)

get_exp_type(exp)

get_glycan_type(exp)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md).

- x:

  A string, the name of the meta data field. If `NULL` (default), a list
  of all meta data fields will be returned.

## Value

The value of the meta data field. If the field does not exist, `NULL`
will be returned.
