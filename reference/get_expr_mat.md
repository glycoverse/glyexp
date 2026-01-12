# Get the expression matrix of an experiment

A `matrix` of expression values with samples as columns and variables as
rows.

## Usage

``` r
get_expr_mat(exp)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

## Value

A matrix of expression values.

## Examples

``` r
get_expr_mat(real_experiment)[1:5, 1:5]
#>                                               C1         C2           C3
#> P08185-N176-Hex(5)HexNAc(4)NeuAc(2)           NA         NA     10655.62
#> P04196-N344-Hex(5)HexNAc(4)NeuAc(1)-1  414080036  609889761  78954431.49
#> P04196-N344-Hex(5)HexNAc(4)            581723113  604842244 167889901.32
#> P04196-N344-Hex(5)HexNAc(4)NeuAc(1)-2 3299649335 2856490652 957651065.86
#> P10909-N291-Hex(6)HexNAc(5)-1           30427048   34294394   6390129.81
#>                                                 H1         H2
#> P08185-N176-Hex(5)HexNAc(4)NeuAc(2)   3.105412e+04         NA
#> P04196-N344-Hex(5)HexNAc(4)NeuAc(1)-1           NA   11724908
#> P04196-N344-Hex(5)HexNAc(4)           6.977076e+08  703566323
#> P04196-N344-Hex(5)HexNAc(4)NeuAc(1)-2 2.600523e+09 3229968280
#> P10909-N291-Hex(6)HexNAc(5)-1         5.159133e+07   37479075
```
