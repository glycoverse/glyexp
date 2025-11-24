# Get the meta data of an experiment

Meta data is some descriptions about the experiment, like the experiment
type ("glycomics" or "glycoproteomics"), or the glycan type ("N",
"O-GalNAc", "O-GlcNAc", "O-Man", "O-Fuc", or "O-Glc").

## Usage

``` r
get_meta_data(exp, x = NULL)

get_exp_type(exp)

get_glycan_type(exp)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- x:

  A string, the name of the meta data field. If `NULL` (default), a list
  of all meta data fields will be returned.

## Value

The value of the meta data field. If the field does not exist, `NULL`
will be returned.

## Examples

``` r
get_meta_data(real_experiment)
#> $exp_type
#> [1] "glycoproteomics"
#> 
#> $glycan_type
#> [1] "N"
#> 
#> $quant_method
#> [1] "label-free"
#> 
get_exp_type(real_experiment)
#> [1] "glycoproteomics"
get_glycan_type(real_experiment)
#> [1] "N"
```
