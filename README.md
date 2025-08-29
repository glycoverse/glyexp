
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glyexp <a href="https://glycoverse.github.io/glyexp/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/glyexp)](https://CRAN.R-project.org/package=glyexp)
[![R-CMD-check](https://github.com/glycoverse/glyexp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/glycoverse/glyexp/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/glycoverse/glyexp/graph/badge.svg)](https://app.codecov.io/gh/glycoverse/glyexp)
<!-- badges: end -->

The goal of glyexp is to manage data in glycoproteomics and glycomics
experiments in a tidy way.

## Installation

You can install the latest release of glyexp from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("glycoverse/glyexp@*release")
```

Or install the development version:

``` r
pak::pak("glycoverse/glyexp")
```

## Documentation

-   🚀 Get started:
    [Here](https://glycoverse.github.io/glyexp/articles/glyexp.html)
-   🔧 dplyr-style data manipulation:
    [Here](https://glycoverse.github.io/glyexp/articles/dplyr-style-functions.html)
-   📚 Reference:
    [Here](https://glycoverse.github.io/glyexp/reference/index.html)

## Role in `glycoverse`

The `experiment()` class provides a consistent interface for
glycoprotemics and glycomics data. All other packages in the
`glycoverse` ecosystem know how to extract information from an
`experiment()` object. So, put your data in an `experiment()` object and
pass it around. Let other packages do the heavy lifting.

## Example

``` r
library(glyexp)
library(magrittr)

# Create a toy experiment
a_little_toy <- toy_experiment
a_little_toy
#> 
#> ── Experiment ──────────────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: group and batch
#> ℹ Variable information fields: protein, peptide, and glycan_composition
```

``` r
get_expr_mat(a_little_toy)
#>    S1 S2 S3 S4 S5 S6
#> V1  1  5  9 13 17 21
#> V2  2  6 10 14 18 22
#> V3  3  7 11 15 19 23
#> V4  4  8 12 16 20 24
```

``` r
get_sample_info(a_little_toy)
#> # A tibble: 6 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
#> 4 S4     B         2
#> 5 S5     B         1
#> 6 S6     B         2
```

``` r
get_var_info(a_little_toy)
#> # A tibble: 4 × 4
#>   variable protein peptide glycan_composition
#>   <chr>    <chr>   <chr>   <chr>             
#> 1 V1       PRO1    PEP1    H5N2              
#> 2 V2       PRO2    PEP2    H5N2              
#> 3 V3       PRO3    PEP3    N3N2              
#> 4 V4       PRO3    PEP4    N3N2
```

``` r
# Filter samples
a_little_toy %>% 
  filter_obs(group == "A") %>%
  filter_var(protein == "PRO1")
#> 
#> ── Experiment ──────────────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 1 variables
#> ℹ Sample information fields: group and batch
#> ℹ Variable information fields: protein, peptide, and glycan_composition
```
