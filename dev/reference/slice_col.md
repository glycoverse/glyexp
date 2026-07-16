# Slice sample or variable information

Slice the sample or variable information of an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
or `SummarizedExperiment`.

These functions provide row-wise slicing operations similar to dplyr's
slice functions. They select rows by position or based on values in
specified columns, and update the expression matrix accordingly to match
the new selection.

- `slice_col()` and `slice_row()`: Select rows by position

- `slice_head_col()` and `slice_head_row()`: Select first n rows

- `slice_tail_col()` and `slice_tail_row()`: Select last n rows

- `slice_sample_col()` and `slice_sample_row()`: Select random n rows

- `slice_max_col()` and `slice_max_row()`: Select rows with highest
  values

- `slice_min_col()` and `slice_min_row()`: Select rows with lowest
  values

## Usage

``` r
slice_col(exp, ...)

slice_row(exp, ...)

slice_head_col(exp, n, prop)

slice_head_row(exp, n, prop)

slice_tail_col(exp, n, prop)

slice_tail_row(exp, n, prop)

slice_sample_col(exp, n, prop, weight_by = NULL, replace = FALSE)

slice_sample_row(exp, n, prop, weight_by = NULL, replace = FALSE)

slice_max_col(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE)

slice_max_row(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE)

slice_min_col(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE)

slice_min_row(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
  or `SummarizedExperiment` object.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  For `slice_*()`, integer row positions. For `slice_max()` and
  `slice_min()`, variables to order by. Other arguments passed to the
  corresponding dplyr function.

- n:

  For `slice_head()`, `slice_tail()`, `slice_sample()`, `slice_max()`,
  and `slice_min()`, the number of rows to select.

- prop:

  For `slice_head()`, `slice_tail()`, `slice_sample()`, `slice_max()`,
  and `slice_min()`, the proportion of rows to select.

- weight_by:

  For `slice_sample()`, sampling weights.

- replace:

  For `slice_sample()`, should sampling be with replacement?

- order_by:

  For `slice_max()` and `slice_min()`, variable to order by.

- with_ties:

  For `slice_max()` and `slice_min()`, should ties be kept?

- na_rm:

  For `slice_max()` and `slice_min()`, should missing values be removed?

## Value

An object of the same class as `exp`.

## Identifier columns

For an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object, `sample` is a physical column in `sample_info`, and `variable`
is a physical column in `var_info`.

For a `SummarizedExperiment`, sample and variable identifiers live in
`colnames(exp)` and `rownames(exp)`, rather than in
[`SummarizedExperiment::colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
or
[`SummarizedExperiment::rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).
Observation verbs expose `colnames(exp)` as a virtual `.sample` column,
and variable verbs expose `rownames(exp)` as a virtual `.variable`
column. These dot-prefixed names distinguish dimension identifiers from
regular metadata columns. After the operation, the virtual column is
removed and its values are written back to the corresponding dimension
names.

Consequently, `sample` in `colData(exp)` and `variable` in
`rowData(exp)` remain ordinary metadata columns. The names `.sample` and
`.variable` are reserved; an input containing either name in the
corresponding metadata raises an error rather than overwriting that
column.

If the corresponding dimension names are `NULL`, the virtual identifier
is unavailable and referring to it raises an error.
`mutate_col(.sample = ...)` or `mutate_row(.variable = ...)` can be used
to create the missing names.

## Examples

``` r
# Add values used for slicing to a bundled experiment
exp <- real_experiment |>
  mutate_col(score = seq_len(dplyr::n())) |>
  mutate_row(value = seq_len(dplyr::n()))

# Select specific rows by position
slice_col(exp, 1, 3, 5)
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 3 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>, value <int>
#> ℹ Column data fields: group <fct>, score <int>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>

# Select first 3 samples
slice_head_col(exp, n = 3)
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 3 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>, value <int>
#> ℹ Column data fields: group <fct>, score <int>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>

# Select last 2 variables
slice_tail_row(exp, n = 2)
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 2 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>, value <int>
#> ℹ Column data fields: group <fct>, score <int>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>

# Select 2 random samples
slice_sample_col(exp, n = 2)
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 2 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>, value <int>
#> ℹ Column data fields: group <fct>, score <int>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>

# Select samples with highest scores
slice_max_col(exp, order_by = score, n = 2)
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 2 samples, 4262 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>, value <int>
#> ℹ Column data fields: group <fct>, score <int>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>

# Select variables with lowest values
slice_min_row(exp, order_by = value, n = 2)
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 2 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>, value <int>
#> ℹ Column data fields: group <fct>, score <int>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
```
