# Join data to sample or variable information

These functions allow you to join additional data to the sample
information or variable information of an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
or `SummarizedExperiment`. They work similarly to
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
and
[`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
while keeping assay dimensions synchronized with the joined metadata.

After joining, the assay dimensions are automatically updated to reflect
any changes in the number of samples or variables.

**Important Notes:**

- The `relationship` parameter is locked to "many-to-one" to ensure that
  the number of observations never increases, which would violate the
  experiment object assumptions.

- `right_join()` and `full_join()` are not supported as they could add
  new observations to the experiment.

## Usage

``` r
left_join_col(exp, y, by = NULL, ...)

inner_join_col(exp, y, by = NULL, ...)

semi_join_col(exp, y, by = NULL, ...)

anti_join_col(exp, y, by = NULL, ...)

left_join_row(exp, y, by = NULL, ...)

inner_join_row(exp, y, by = NULL, ...)

semi_join_row(exp, y, by = NULL, ...)

anti_join_row(exp, y, by = NULL, ...)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
  or `SummarizedExperiment` object.

- y:

  A data frame to join to `sample_info` or `var_info`.

- by:

  A join specification created with
  [`dplyr::join_by()`](https://dplyr.tidyverse.org/reference/join_by.html),
  or a character vector of variables to join by. See
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  for details.

- ...:

  Other arguments passed to the underlying dplyr join function, except
  `relationship` which is locked to "many-to-one".

## Value

An object of the same class as `exp`, with updated sample or variable
information.

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

## See also

[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
