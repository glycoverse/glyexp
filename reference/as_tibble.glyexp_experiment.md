# Convert an experiment to a tibble

Convert an experiment object to a tibble of "tidy" format. That is, each
row is a unique combination of "sample" and "variable", with the
observation (the abundance) in the "value" column. Additional columns in
the sample and variable information are included. This format is also
known as the "long" format.

Usually you don't want all columns in the sample information or variable
information tibbles to be included in the output tibble, as this will
make the output tibble very "wide". You can specify which columns to
include in the output tibble by passing the column names to the
`sample_cols` and `var_cols` arguments.
\<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
syntax is used here. By default, all columns are included.

## Usage

``` r
# S3 method for class 'glyexp_experiment'
as_tibble(
  x,
  sample_cols = tidyselect::everything(),
  var_cols = tidyselect::everything(),
  ...
)
```

## Arguments

- x:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- sample_cols:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Columns to include from the sample information tibble.

- var_cols:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Columns to include from the variable information tibble.

- ...:

  Ignored.

## Value

A tibble.

## Examples

``` r
library(tibble)

# Create a toy experiment for demonstration
toy_exp <- toy_experiment
toy_exp
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>

# Convert the experiment to a tibble
as_tibble(toy_exp)
#> # A tibble: 24 × 8
#>    sample group batch variable protein peptide glycan_composition value
#>    <chr>  <chr> <dbl> <chr>    <chr>   <chr>   <chr>              <int>
#>  1 S1     A         1 V1       PRO1    PEP1    H5N2                   1
#>  2 S2     A         2 V1       PRO1    PEP1    H5N2                   5
#>  3 S3     A         1 V1       PRO1    PEP1    H5N2                   9
#>  4 S4     B         2 V1       PRO1    PEP1    H5N2                  13
#>  5 S5     B         1 V1       PRO1    PEP1    H5N2                  17
#>  6 S6     B         2 V1       PRO1    PEP1    H5N2                  21
#>  7 S1     A         1 V2       PRO2    PEP2    H5N2                   2
#>  8 S2     A         2 V2       PRO2    PEP2    H5N2                   6
#>  9 S3     A         1 V2       PRO2    PEP2    H5N2                  10
#> 10 S4     B         2 V2       PRO2    PEP2    H5N2                  14
#> # ℹ 14 more rows

# specify columns to include
as_tibble(toy_exp, sample_cols = group, var_cols = c(protein, peptide))
#> # A tibble: 24 × 6
#>    sample group variable protein peptide value
#>    <chr>  <chr> <chr>    <chr>   <chr>   <int>
#>  1 S1     A     V1       PRO1    PEP1        1
#>  2 S2     A     V1       PRO1    PEP1        5
#>  3 S3     A     V1       PRO1    PEP1        9
#>  4 S4     B     V1       PRO1    PEP1       13
#>  5 S5     B     V1       PRO1    PEP1       17
#>  6 S6     B     V1       PRO1    PEP1       21
#>  7 S1     A     V2       PRO2    PEP2        2
#>  8 S2     A     V2       PRO2    PEP2        6
#>  9 S3     A     V2       PRO2    PEP2       10
#> 10 S4     B     V2       PRO2    PEP2       14
#> # ℹ 14 more rows
```
