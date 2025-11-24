# Merge two experiments

Merges two
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects into a single object.

**Expression matrix:** Expression matrices are merged by matching
variables. For variables that are present in only one of the
experiments, the corresponding values in the expression matrix are set
to `NA`.

**Sample information:** Column names and column types must be identical
(order does not matter). No overlap is allowed for sample names.

**Variable information:** Column names and column types must be
identical (order does not matter). Variable names are ignored. The
identity of variables is determined by all other columns in the variable
information. If row A in the first `var_info` is identical (order does
not matter) to row B in the second `var_info`, they are considered the
same variable. Therefore, please make sure that each row has a unique
combination of values (except for `variable`), otherwise the function
cannot determine which variable in `x` is identical to which variable in
`y`. To ensure uniqueness, you can use `glyclean::aggregate()`.

**Metadata:** Metadata is taken from the first experiment. This is the
only place where the order of `x` and `y` matters.

## Usage

``` r
# S3 method for class 'glyexp_experiment'
merge(x, y, ...)
```

## Arguments

- x, y:

  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  objects to merge

- ...:

  Not used

## Value

A new
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object

## Variable matching

In the variable information tibble, the `variable` column is just
arbitrary unique identifier, while the real identity of a variable is
determined by all other columns. For example, if the variable
information tibble has the following columns:

    variable glycan_composition protein protein_site
    V1       Hex(2)HexNAc(1)    P1      2
    V2       Hex(2)HexNAc(1)    P2      3
    V3       Hex(2)HexNAc(2)    P2      3
    V4       Hex(2)HexNAc(2)    P2      3

We know that V1, V2, and V3 are all different variables, but V3 and V4
are the same variable (they have the same glycan composition, protein,
and protein site).

During the merge, the function will use all columns in the variable
information tibble except for `variable` to match variables. This means
that if the combination of all other columns is not unique, the function
cannot determine the identity of the variables.

To ensure uniqueness, you can use `glyclean::aggregate()`, which merges
the expression levels of variables with the same identity.

After the merge, a new `variable` column is added to the variable
information tibble, and used as the rownames of the expression matrix.

## Variable and sample orders

The order of variables and samples in the merged experiment is
deterministic.

For samples, as no overlapping is allowed for sample names, the new
sample names are the union of the sample names in `x` and `y`, orders
reserved.

For variables, the new variables are:

1.  First, all variables in `x`, with the same order as in `x`.

2.  Then, all variables in `y` while not in `x`, with the same order as
    in `y`.

Note that for variables, we refer to the identity of variables, not the
variable names in the `variable` column.

## Examples

``` r
# Merging is most useful with experiments from different batches.
# Here we just demonstrate the usage.

# Create experiments to be merged
exp1 <- toy_experiment
exp2 <- toy_experiment |>
  mutate_obs(sample = paste0("S", 7:12))
exp3 <- toy_experiment |>
  mutate_obs(sample = paste0("S", 13:18))

# Merge two experiments
merge(exp1, exp2)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 12 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>

# Merge multiple experiments
Reduce(merge, list(exp1, exp2, exp3))
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 18 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
```
