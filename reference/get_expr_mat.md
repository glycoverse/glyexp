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
#>             C1         C2           C3           H1         H2
#> GP1         NA         NA     10655.62 3.105412e+04         NA
#> GP2  414080036  609889761  78954431.49           NA   11724908
#> GP3  581723113  604842244 167889901.32 6.977076e+08  703566323
#> GP4 3299649335 2856490652 957651065.86 2.600523e+09 3229968280
#> GP5   30427048   34294394   6390129.81 5.159133e+07   37479075
```
