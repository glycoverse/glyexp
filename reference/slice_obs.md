# Slice sample or variable information

Slice the sample or variable information tibble of an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

These functions provide row-wise slicing operations similar to dplyr's
slice functions. They select rows by position or based on values in
specified columns, and update the expression matrix accordingly to match
the new selection.

- `slice_obs()` and `slice_var()`: Select rows by position

- `slice_head_obs()` and `slice_head_var()`: Select first n rows

- `slice_tail_obs()` and `slice_tail_var()`: Select last n rows

- `slice_sample_obs()` and `slice_sample_var()`: Select random n rows

- `slice_max_obs()` and `slice_max_var()`: Select rows with highest
  values

- `slice_min_obs()` and `slice_min_var()`: Select rows with lowest
  values

## Usage

``` r
slice_obs(exp, ...)

slice_var(exp, ...)

slice_head_obs(exp, n, prop)

slice_head_var(exp, n, prop)

slice_tail_obs(exp, n, prop)

slice_tail_var(exp, n, prop)

slice_sample_obs(exp, n, prop, weight_by = NULL, replace = FALSE)

slice_sample_var(exp, n, prop, weight_by = NULL, replace = FALSE)

slice_max_obs(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE)

slice_max_var(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE)

slice_min_obs(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE)

slice_min_var(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  For `slice_*()`, integer row positions. For
  [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html) and
  [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
  variables to order by. Other arguments passed to the corresponding
  dplyr function.

- n:

  For
  [`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html), and
  [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html), the
  number of rows to select.

- prop:

  For
  [`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html), and
  [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html), the
  proportion of rows to select.

- weight_by:

  For
  [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html),
  sampling weights.

- replace:

  For
  [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html),
  should sampling be with replacement?

- order_by:

  For [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html)
  and [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
  variable to order by.

- with_ties:

  For [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html)
  and [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
  should ties be kept?

- na_rm:

  For [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html)
  and [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
  should missing values be removed?

## Value

A new
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object.

## Examples

``` r
# Create a toy experiment for demonstration
exp <- toy_experiment |>
  mutate_obs(score = c(10, 20, 30, 15, 25, 35)) |>
  mutate_var(value = c(5, 10, 15, 8))

# Select specific rows by position
slice_obs(exp, 1, 3, 5)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>, score <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>, value <dbl>

# Select first 3 samples
slice_head_obs(exp, n = 3)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>, score <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>, value <dbl>

# Select last 2 variables
slice_tail_var(exp, n = 2)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 2 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>, score <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>, value <dbl>

# Select 2 random samples
slice_sample_obs(exp, n = 2)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 2 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>, score <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>, value <dbl>

# Select samples with highest scores
slice_max_obs(exp, order_by = score, n = 2)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 2 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>, score <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>, value <dbl>

# Select variables with lowest values
slice_min_var(exp, order_by = value, n = 2)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 2 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>, score <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>, value <dbl>
```
