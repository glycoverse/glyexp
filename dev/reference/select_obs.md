# Select columns of the sample or variable information tibble

These two functions provide a way to trimming down the sample or
variable information tibble of an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
to only the columns of interest.

The same syntax as
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
is used. For example, to get a new
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
with only the "sample" and "group" columns in the sample information
tibble, use `select_obs(exp, group)`. Note that you don't need to (and
you can't) explicitly select or deselect the `sample` column in
`sample_info`. The same applies to the `variable` column in `var_info`.
Whatever the selection expression is, the `sample` or `variable` column
will always be kept.

## Usage

``` r
select_obs(exp, ...)

select_var(exp, ...)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Column names to select. If empty, all columns except the `sample` or
  `variable` column will be discarded.

## Value

An new
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object.

## Details

When using `select_var()` with `dplyr`, you may encounter package
conflicts. `dplyr` also has a function called `select_var()` that has
been deprecated for over two years. If you encounter package conflicts,
use the following code to resolve them:

    conflicted::conflicts_prefer(glyexp::select_var)

## Examples

``` r
toy_exp <- toy_experiment

toy_exp_2 <- toy_exp |>
  select_obs(group) |>
  select_var(protein, peptide)

get_sample_info(toy_exp_2)
#> # A tibble: 6 × 2
#>   sample group
#>   <chr>  <chr>
#> 1 S1     A    
#> 2 S2     A    
#> 3 S3     A    
#> 4 S4     B    
#> 5 S5     B    
#> 6 S6     B    
get_var_info(toy_exp_2)
#> # A tibble: 4 × 3
#>   variable protein peptide
#>   <chr>    <chr>   <chr>  
#> 1 V1       PRO1    PEP1   
#> 2 V2       PRO2    PEP2   
#> 3 V3       PRO3    PEP3   
#> 4 V4       PRO3    PEP4   
```
