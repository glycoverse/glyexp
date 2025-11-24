# Get the variable information of an experiment

A `tibble` of variable information, with the first column being
"variable".

## Usage

``` r
get_var_info(exp)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

## Value

A tibble of variable information.

## Examples

``` r
get_var_info(real_experiment)
#> # A tibble: 4,262 × 8
#>    variable peptide   peptide_site protein protein_site gene  glycan_composition
#>    <chr>    <chr>            <int> <chr>          <int> <chr> <comp>            
#>  1 GP1      JKTQGK               1 P08185           176 SERP… Hex(5)HexNAc(4)Ne…
#>  2 GP2      HSHNJJSS…            5 P04196           344 HRG   Hex(5)HexNAc(4)Ne…
#>  3 GP3      HSHNJJSS…            5 P04196           344 HRG   Hex(5)HexNAc(4)   
#>  4 GP4      HSHNJJSS…            5 P04196           344 HRG   Hex(5)HexNAc(4)Ne…
#>  5 GP5      HJSTGCLR             2 P10909           291 CLU   Hex(6)HexNAc(5)   
#>  6 GP6      HSHNJJSS…            5 P04196           344 HRG   Hex(5)HexNAc(4)Ne…
#>  7 GP7      HSHNJJSS…            6 P04196           345 HRG   Hex(5)HexNAc(4)   
#>  8 GP8      HSHNJJSS…            5 P04196           344 HRG   Hex(5)HexNAc(4)dH…
#>  9 GP9      HSHNJJSS…            5 P04196           344 HRG   Hex(4)HexNAc(3)   
#> 10 GP10     HSHNJJSS…            5 P04196           344 HRG   Hex(4)HexNAc(4)Ne…
#> # ℹ 4,252 more rows
#> # ℹ 1 more variable: glycan_structure <struct>
```
