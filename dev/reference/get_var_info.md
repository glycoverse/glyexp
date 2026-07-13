# Get the variable information of an experiment

A `tibble` of variable information, with the first column being
"variable".

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
as the default data container.

## Usage

``` r
get_var_info(exp)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md).

## Value

A tibble of variable information.

## Examples

``` r
get_var_info(real_experiment)
#> # A tibble: 4,262 × 8
#>    variable   peptide peptide_site protein protein_site gene  glycan_composition
#>    <chr>      <chr>          <int> <chr>          <int> <chr> <comp>            
#>  1 P08185-17… NKTQGK             1 P08185           176 SERP… Hex(5)HexNAc(4)Ne…
#>  2 P04196-34… HSHNNN…            5 P04196           344 HRG   Hex(5)HexNAc(4)Ne…
#>  3 P04196-34… HSHNNN…            5 P04196           344 HRG   Hex(5)HexNAc(4)   
#>  4 P04196-34… HSHNNN…            5 P04196           344 HRG   Hex(5)HexNAc(4)Ne…
#>  5 P10909-29… HNSTGC…            2 P10909           291 CLU   Hex(6)HexNAc(5)   
#>  6 P04196-34… HSHNNN…            5 P04196           344 HRG   Hex(5)HexNAc(4)Ne…
#>  7 P04196-34… HSHNNN…            6 P04196           345 HRG   Hex(5)HexNAc(4)   
#>  8 P04196-34… HSHNNN…            5 P04196           344 HRG   Hex(5)HexNAc(4)dH…
#>  9 P04196-34… HSHNNN…            5 P04196           344 HRG   Hex(4)HexNAc(3)   
#> 10 P04196-34… HSHNNN…            5 P04196           344 HRG   Hex(4)HexNAc(4)Ne…
#> # ℹ 4,252 more rows
#> # ℹ 1 more variable: glycan_structure <struct>
```
