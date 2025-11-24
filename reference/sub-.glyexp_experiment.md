# Subsetting experiments

Getting a subset of an experiment object. Subsetting is first done on
the expression matrix, then the sample information and variable
information tibbles are filtered and ordered accordingly.

Syntax for `[` is similar to subsetting a matrix, with some differences:

- Both row and column indices are required, i.e. `exp[i]` is not
  allowed, but `exp[i, ]` and `exp[, j]` are allowed.

- `drop` argument is not supported. Subsetting an experiment always
  returns an new experiment, even if it has only one sample or one
  variable.

- Renaming the subsetted experiment is no longer supported.

Assigning to a subset of an experiment is not allowed, i.e.,
`exp[1, 1[ <- 0` will raise an error. You can create a new experiment
with new data if needed.

## Usage

``` r
# S3 method for class 'glyexp_experiment'
x[i, j, ...]

# S3 method for class 'glyexp_experiment'
x[i, j, ...] <- value
```

## Arguments

- x:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- i, j:

  Row (variable) and column (sample) indices to subset.

- ...:

  Ignored.

- value:

  Ignored.

## Value

An
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object.

## Examples

``` r
# Create a toy experiment for demonstration
exp <- toy_experiment

# Subsetting single samples
exp[, "S1"]
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 1 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
exp[, 1]
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 1 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>

# Subsetting single variables
exp["V1", ]
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 1 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
exp[1, ]
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 1 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>

# Subsetting multiple samples and variables
exp[c("V1", "V2"), c("S2", "S3")]
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 2 samples, 2 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
exp[c(1, 2), c(2, 3)]
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 2 samples, 2 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>

# Create a copy
exp[, ]
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
```
