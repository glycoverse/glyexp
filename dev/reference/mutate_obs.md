# Mutate sample or variable information

Mutate the sample or variable information of an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
or `SummarizedExperiment`.

The same syntax as
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
is used. For example, to add a new column to the sample information
tibble, use `mutate_obs(exp, new_column = value)`. This actually calls
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
on the sample information tibble with `new_column = value`.

If an identifier column is modified, its new values must be unique;
otherwise, an error is thrown. The assay column names or row names will
be updated accordingly.

## Usage

``` r
mutate_obs(exp, ...)

mutate_var(exp, ...)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
  or `SummarizedExperiment` object.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs, passed to
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  internally.

## Value

An object of the same class as `exp`.

## Identifier columns

For an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object, `sample` is a physical column in `sample_info`, and `variable`
is a physical column in `var_info`.

For a `SummarizedExperiment`, sample and variable identifiers live in
`colnames(exp)` and `rownames(exp)`, rather than in
[`SummarizedExperiment::colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
or
[`SummarizedExperiment::rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).
Observation verbs expose `colnames(exp)` as a virtual `.sample` column,
and variable verbs expose `rownames(exp)` as a virtual `.variable`
column. These dot-prefixed names distinguish dimension identifiers from
regular metadata columns. After the operation, the virtual column is
removed and its values are written back to the corresponding dimension
names.

Consequently, `sample` in `colData(exp)` and `variable` in
`rowData(exp)` remain ordinary metadata columns. The names `.sample` and
`.variable` are reserved; an input containing either name in the
corresponding metadata raises an error rather than overwriting that
column.

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

# SummarizedExperiment identifiers use virtual dot-prefixed columns
se <- as_se(toy_experiment)
mutate_obs(se, .sample = paste0("new_", .sample))
#> class: SummarizedExperiment 
#> dim: 4 6 
#> metadata(2): exp_type glycan_type
#> assays(1): abundance
#> rownames(4): V1 V2 V3 V4
#> rowData names(3): protein peptide glycan_composition
#> colnames(6): new_S1 new_S2 ... new_S5 new_S6
#> colData names(2): group batch
mutate_var(se, .variable = paste0("new_", .variable))
#> class: SummarizedExperiment 
#> dim: 4 6 
#> metadata(2): exp_type glycan_type
#> assays(1): abundance
#> rownames(4): new_V1 new_V2 new_V3 new_V4
#> rowData names(3): protein peptide glycan_composition
#> colnames(6): S1 S2 ... S5 S6
#> colData names(2): group batch
```
