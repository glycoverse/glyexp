# Mutate sample or variable information

Mutate the sample or variable information tibble of an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

The same syntax as
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
is used. For example, to add a new column to the sample information
tibble, use `mutate_obs(exp, new_column = value)`. This actually calls
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
on the sample information tibble with `new_column = value`.

If the `sample` column in `sample_info` or the `variable` column in
`var_info` is to be modified, the new column must be unique, otherwise
an error is thrown. The column names or row names of `expr_mat` will be
updated accordingly.

## Usage

``` r
mutate_obs(exp, ...)

mutate_var(exp, ...)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs, passed to
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  internally.

## Value

An new
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object.

## Examples

``` r
# Create a toy experiment for demonstration
exp <- toy_experiment |>
  mutate_var(type = c("X", "X", "Y", "Y"))

# Add a new column to sample information tibble or variable information tibble
exp |>
  mutate_obs(new_column = c(1, 2, 3, 4, 5, 6)) |>
  get_sample_info()
#> # A tibble: 6 × 4
#>   sample group batch new_column
#>   <chr>  <chr> <dbl>      <dbl>
#> 1 S1     A         1          1
#> 2 S2     A         2          2
#> 3 S3     A         1          3
#> 4 S4     B         2          4
#> 5 S5     B         1          5
#> 6 S6     B         2          6

exp |>
  mutate_var(new_column = c("A", "A", "B", "B")) |>
  get_var_info()
#> # A tibble: 4 × 6
#>   variable protein peptide glycan_composition type  new_column
#>   <chr>    <chr>   <chr>   <chr>              <chr> <chr>     
#> 1 V1       PRO1    PEP1    H5N2               X     A         
#> 2 V2       PRO2    PEP2    H5N2               X     A         
#> 3 V3       PRO3    PEP3    H3N2               Y     B         
#> 4 V4       PRO3    PEP4    H3N2               Y     B         

# Modify existing columns
exp |>
  mutate_obs(group = dplyr::if_else(group == "A", "good", "bad")) |>
  get_sample_info()
#> # A tibble: 6 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     good      1
#> 2 S2     good      2
#> 3 S3     good      1
#> 4 S4     bad       2
#> 5 S5     bad       1
#> 6 S6     bad       2

exp |>
  mutate_var(type = dplyr::if_else(type == "X", "good", "bad")) |>
  get_var_info()
#> # A tibble: 4 × 5
#>   variable protein peptide glycan_composition type 
#>   <chr>    <chr>   <chr>   <chr>              <chr>
#> 1 V1       PRO1    PEP1    H5N2               good 
#> 2 V2       PRO2    PEP2    H5N2               good 
#> 3 V3       PRO3    PEP3    H3N2               bad  
#> 4 V4       PRO3    PEP4    H3N2               bad  

# Modify the `sample` column in sample information tibble
new_exp <- mutate_obs(exp, sample = c("SI", "SII", "SIII", "SIV", "SV", "SVI"))
get_sample_info(new_exp)
#> # A tibble: 6 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 SI     A         1
#> 2 SII    A         2
#> 3 SIII   A         1
#> 4 SIV    B         2
#> 5 SV     B         1
#> 6 SVI    B         2
get_expr_mat(new_exp)
#>    SI SII SIII SIV SV SVI
#> V1  1   5    9  13 17  21
#> V2  2   6   10  14 18  22
#> V3  3   7   11  15 19  23
#> V4  4   8   12  16 20  24

# Modify the `variable` column in variable information tibble
new_exp <- mutate_var(exp, variable = c("VI", "VII", "VIII", "VIV"))
get_var_info(new_exp)
#> # A tibble: 4 × 5
#>   variable protein peptide glycan_composition type 
#>   <chr>    <chr>   <chr>   <chr>              <chr>
#> 1 VI       PRO1    PEP1    H5N2               X    
#> 2 VII      PRO2    PEP2    H5N2               X    
#> 3 VIII     PRO3    PEP3    H3N2               Y    
#> 4 VIV      PRO3    PEP4    H3N2               Y    
get_expr_mat(new_exp)
#>      S1 S2 S3 S4 S5 S6
#> VI    1  5  9 13 17 21
#> VII   2  6 10 14 18 22
#> VIII  3  7 11 15 19 23
#> VIV   4  8 12 16 20 24
```
