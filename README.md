
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

Provides a tidy data framework for managing glycoproteomics and
glycomics experimental data. The core features are the `GlycomicSE` and
`GlycoproteomicSE` classes, which extend `SummarizedExperiment` with
validated glycomics and glycoproteomics schemas. They integrate
expression matrices, molecular annotations (proteins, peptides, glycan
compositions, and more), and sample metadata (groups, batches, and
clinical variables). The package enforces data consistency, validates
column types according to experiment types (glycomics, glycoproteomics,
traitomics, traitproteomics), and provides dplyr-style data manipulation
functions (filter, mutate, select, arrange, slice, join) for seamless
data wrangling.

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

- 🚀 Get started:
  [Here](https://glycoverse.github.io/glyexp/articles/glyexp.html)
- 🔧 dplyr-style data manipulation:
  [Here](https://glycoverse.github.io/glyexp/articles/dplyr-style-functions.html)
- 📚 Reference:
  [Here](https://glycoverse.github.io/glyexp/reference/index.html)

## Role in `glycoverse`

`GlycomicSE` and `GlycoproteomicSE` provide consistent interfaces for
glycomics and glycoproteomics data. Other packages in the `glycoverse`
ecosystem can operate on these containers directly. Use them to pass
validated data between analysis steps. Let other packages do the heavy
lifting.

## Example

``` r
library(glyexp)
suppressPackageStartupMessages(library(SummarizedExperiment))

# Inspect a bundled experiment
real_experiment
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
```

``` r
assay(real_experiment)[1:5, 1:3]
#>                                              C1         C2           C3
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)           NA         NA     10655.62
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1  414080036  609889761  78954431.49
#> P04196-344-Hex(5)HexNAc(4)            581723113  604842244 167889901.32
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2 3299649335 2856490652 957651065.86
#> P10909-291-Hex(6)HexNAc(5)-1           30427048   34294394   6390129.81
```

``` r
head(colData(real_experiment))
#> DataFrame with 6 rows and 1 column
#>       group
#>    <factor>
#> C1        C
#> C2        C
#> C3        C
#> H1        H
#> H2        H
#> H3        H
```

``` r
head(rowData(real_experiment))
#> DataFrame with 6 rows and 7 columns
#>                                             peptide peptide_site     protein
#>                                         <character>    <integer> <character>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)           NKTQGK            1      P08185
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1 HSHNNNSSDLHPHK            5      P04196
#> P04196-344-Hex(5)HexNAc(4)           HSHNNNSSDLHPHK            5      P04196
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2 HSHNNNSSDLHPHK            5      P04196
#> P10909-291-Hex(6)HexNAc(5)-1               HNSTGCLR            2      P10909
#> P04196-344-Hex(5)HexNAc(4)NeuAc(2)   HSHNNNSSDLHPHK            5      P04196
#>                                      protein_site        gene
#>                                         <integer> <character>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)            176    SERPINA6
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1          344         HRG
#> P04196-344-Hex(5)HexNAc(4)                    344         HRG
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2          344         HRG
#> P10909-291-Hex(6)HexNAc(5)-1                  291         CLU
#> P04196-344-Hex(5)HexNAc(4)NeuAc(2)            344         HRG
#>                                          glycan_composition
#>                                       <glyrepr_composition>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)   Hex(5)HexNAc(4)NeuAc..
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1 Hex(5)HexNAc(4)NeuAc..
#> P04196-344-Hex(5)HexNAc(4)                  Hex(5)HexNAc(4)
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2 Hex(5)HexNAc(4)NeuAc..
#> P10909-291-Hex(6)HexNAc(5)-1                Hex(6)HexNAc(5)
#> P04196-344-Hex(5)HexNAc(4)NeuAc(2)   Hex(5)HexNAc(4)NeuAc..
#>                                            glycan_structure
#>                                         <glyrepr_structure>
#> P08185-176-Hex(5)HexNAc(4)NeuAc(2)   NeuAc(??-?)Hex(??-?)..
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-1 NeuAc(??-?)Hex(??-?)..
#> P04196-344-Hex(5)HexNAc(4)           Hex(??-?)HexNAc(??-?..
#> P04196-344-Hex(5)HexNAc(4)NeuAc(1)-2 NeuAc(??-?)Hex(??-?)..
#> P10909-291-Hex(6)HexNAc(5)-1         Hex(??-?)HexNAc(??-?..
#> P04196-344-Hex(5)HexNAc(4)NeuAc(2)   NeuAc(??-?)Hex(??-?)..
```

``` r
# Filter samples
real_experiment |>
  filter_col(group == "H")
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 3 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
```
