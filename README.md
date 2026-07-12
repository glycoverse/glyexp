
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glyexp <a href="https://glycoverse.github.io/glyexp/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-universe
version](https://glycoverse.r-universe.dev/glyexp/badges/version)](https://glycoverse.r-universe.dev/glyexp)
[![R-CMD-check](https://github.com/glycoverse/glyexp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/glycoverse/glyexp/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/glycoverse/glyexp/graph/badge.svg)](https://app.codecov.io/gh/glycoverse/glyexp)
<!-- badges: end -->

Provides `SummarizedExperiment`-based containers for glycoproteomics and
glycomics experimental data. `GlycomicSE()` and `GlycoproteomicSE()`
validate glycan-specific annotations while preserving the standard
assay, `rowData`, `colData`, and metadata interfaces.

The legacy `experiment()` API and its related helpers are deprecated but
remain available for a staged migration of the `glycoverse` ecosystem.

## Installation

### Install glycoverse

We recommend installing the meta-package
[glycoverse](https://github.com/glycoverse/glycoverse), which includes
this package and other core glycoverse packages.

### Install glyexp alone

If you don’t want to install all glycoverse packages, you can only
install glyexp.

You can install the latest release of glyexp from
[r-universe](https://glycoverse.r-universe.dev/glyexp)
(**recommended**):

``` r
# install.packages("pak")
pak::repo_add(glycoverse = "https://glycoverse.r-universe.dev")
pak::pkg_install("glyexp")
```

Or from [GitHub](https://github.com/glycoverse/glyexp):

``` r
pak::pkg_install("glycoverse/glyexp@*release")
```

Or install the development version (NOT recommended):

``` r
pak::pkg_install("glycoverse/glyexp")
```

**Note:** Tips and troubleshooting for the meta-package
[glycoverse](https://github.com/glycoverse/glycoverse) are also
applicable here: [Installation of
glycoverse](https://github.com/glycoverse/glycoverse#installation).

## Documentation

-   🚀 Get started:
    [Here](https://glycoverse.github.io/glyexp/articles/glyexp.html)
-   🔧 dplyr-style data manipulation:
    [Here](https://glycoverse.github.io/glyexp/articles/dplyr-style-functions.html)
-   📚 Reference:
    [Here](https://glycoverse.github.io/glyexp/reference/index.html)

## Role in `glycoverse`

`GlycomicSE()` and `GlycoproteomicSE()` are the recommended data
containers for new workflows. They are `SummarizedExperiment`
subclasses, so other packages can use standard Bioconductor assay and
annotation interfaces directly.

## Example

``` r
library(glyexp)

# Create a GlycomicSE object
abundance <- matrix(1:4, nrow = 2, dimnames = list(c("G1", "G2"), c("S1", "S2")))
glycomic_se <- GlycomicSE(
  abundance,
  rowData = S4Vectors::DataFrame(
    glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 2)
  ),
  metadata = list(glycan_type = "N")
)

SummarizedExperiment::assay(glycomic_se)
#>    S1 S2
#> G1  1  3
#> G2  2  4
SummarizedExperiment::rowData(glycomic_se)
#> DataFrame with 2 rows and 1 column
#>       glycan_composition
#>    <glyrepr_composition>
#> G1                Hex(1)
#> G2                Hex(1)
```
