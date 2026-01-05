# Filter samples or variables of an experiment

Getting a subset of an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
by filtering samples or variables.

The same syntax as
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
is used. For example, to get a subset of an experiment keeping only "HC"
samples, use `filter_obs(exp, group == "HC")`. This actually calls
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
on the sample information tibble with condition `group == "HC"`, and
then updates the expression matrix accordingly.

## Usage

``` r
filter_obs(exp, ..., .drop_levels = FALSE)

filter_var(exp, ..., .drop_levels = FALSE)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Expression to filter samples or variables. passed to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  internally.

- .drop_levels:

  Logical. If `TRUE`, drop unused factor levels for columns referenced
  in the filtering expressions.

## Value

An new
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object.

## Examples

``` r
# Create a toy experiment for demonstration
exp <- toy_experiment |>
  mutate_var(type = c("X", "X", "Y", "Y"))

# Filter samples
sub_exp_1 <- filter_obs(exp, group == "A")
get_sample_info(sub_exp_1)
#> # A tibble: 3 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
get_expr_mat(sub_exp_1)
#>    S1 S2 S3
#> V1  1  5  9
#> V2  2  6 10
#> V3  3  7 11
#> V4  4  8 12

# Filter variables
sub_exp_2 <- filter_var(exp, type == "X")
get_var_info(sub_exp_2)
#> # A tibble: 2 × 5
#>   variable protein peptide glycan_composition type 
#>   <chr>    <chr>   <chr>   <chr>              <chr>
#> 1 V1       PRO1    PEP1    H5N2               X    
#> 2 V2       PRO2    PEP2    H5N2               X    
get_expr_mat(sub_exp_2)
#>    S1 S2 S3 S4 S5 S6
#> V1  1  5  9 13 17 21
#> V2  2  6 10 14 18 22

# Use pipe
sub_exp_3 <- exp |>
  filter_obs(group == "A") |>
  filter_var(type == "X")
get_sample_info(sub_exp_3)
#> # A tibble: 3 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
get_var_info(sub_exp_3)
#> # A tibble: 2 × 5
#>   variable protein peptide glycan_composition type 
#>   <chr>    <chr>   <chr>   <chr>              <chr>
#> 1 V1       PRO1    PEP1    H5N2               X    
#> 2 V2       PRO2    PEP2    H5N2               X    
get_expr_mat(sub_exp_3)
#>    S1 S2 S3
#> V1  1  5  9
#> V2  2  6 10
```
