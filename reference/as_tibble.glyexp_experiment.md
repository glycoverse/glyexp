# Convert an experiment to a tibble

Convert an experiment object to a tibble of "tidy" format. That is, each
row is a unique combination of "sample" and "variable", with the
observation (the abundance) in the "value" column. Additional columns in
the sample and variable information are included. This format is also
known as the "long" format.

Usually you don't want all columns in the sample information or variable
information tibbles to be included in the output tibble, as this will
make the output tibble very "wide". You can specify which columns to
include in the output tibble by passing the column names to the
`sample_cols` and `var_cols` arguments.
\<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
syntax is used here. By default, all columns are included.

**\[deprecated\]**

This legacy API is retained temporarily for compatibility. Use
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
or
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.md)
as the default data container.

## Usage

``` r
# S3 method for class 'glyexp_experiment'
as_tibble(
  x,
  sample_cols = tidyselect::everything(),
  var_cols = tidyselect::everything(),
  ...
)
```

## Arguments

- x:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- sample_cols:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Columns to include from the sample information tibble.

- var_cols:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Columns to include from the variable information tibble.

- ...:

  Ignored.

## Value

A tibble.
