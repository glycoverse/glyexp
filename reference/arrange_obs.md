# Arrange sample or variable information

Arrange the sample or variable information tibble of an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

The same syntax as
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
is used. For example, to arrange samples by the "group" column, use
`arrange_obs(exp, group)`. This actually calls
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
on the sample information tibble with the `group` column, and then
updates the expression matrix accordingly to match the new order.

## Usage

``` r
arrange_obs(exp, ...)

arrange_var(exp, ...)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables to arrange by, passed to
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
  internally.

## Value

An new
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object.

## Examples

``` r
# Create a toy experiment for demonstration
exp <- toy_experiment |>
  mutate_var(type = c("Y", "X", "Z", "Y"))

# Arrange samples by group column
arranged_exp <- arrange_obs(exp, group)
get_sample_info(arranged_exp)
#> # A tibble: 6 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
#> 4 S4     B         2
#> 5 S5     B         1
#> 6 S6     B         2
get_expr_mat(arranged_exp)
#>    S1 S2 S3 S4 S5 S6
#> V1  1  5  9 13 17 21
#> V2  2  6 10 14 18 22
#> V3  3  7 11 15 19 23
#> V4  4  8 12 16 20 24

# Arrange variables by type column
arranged_exp <- arrange_var(exp, type)
get_var_info(arranged_exp)
#> # A tibble: 4 × 5
#>   variable protein peptide glycan_composition type 
#>   <chr>    <chr>   <chr>   <chr>              <chr>
#> 1 V2       PRO2    PEP2    H5N2               X    
#> 2 V1       PRO1    PEP1    H5N2               Y    
#> 3 V4       PRO3    PEP4    H3N2               Y    
#> 4 V3       PRO3    PEP3    H3N2               Z    
get_expr_mat(arranged_exp)
#>    S1 S2 S3 S4 S5 S6
#> V2  2  6 10 14 18 22
#> V1  1  5  9 13 17 21
#> V4  4  8 12 16 20 24
#> V3  3  7 11 15 19 23

# Arrange by multiple columns
arrange_obs(exp, group, sample)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>, type <chr>
get_sample_info(arranged_exp)
#> # A tibble: 6 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
#> 4 S4     B         2
#> 5 S5     B         1
#> 6 S6     B         2
get_expr_mat(arranged_exp)
#>    S1 S2 S3 S4 S5 S6
#> V2  2  6 10 14 18 22
#> V1  1  5  9 13 17 21
#> V4  4  8 12 16 20 24
#> V3  3  7 11 15 19 23
```
