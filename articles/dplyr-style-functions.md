# dplyr-Style Functions

This vignette describes glyexp’s dplyr-style functions for synchronized
data manipulation.
[SummarizedExperiment](https://bioconductor.org/packages//release/bioc/html/SummarizedExperiment.html)
provides basic ways to inspect and manipulate you SE objects.
[dplyr](https://cran.r-project.org/web/packages/dplyr/index.html) and
[tidyr](https://cran.r-project.org/web/packages/tidyr/index.html)
provides various verb functions to manipulate tabels. `glyexp` tries to
bridge up the two world, just like
[tidySummarizedExperiment](https://bioconductor.posit.co/packages/release/bioc/html/tidySummarizedExperiment.html),
but in a different way. While `tidySummarizedExperiment` re-use `dplyr`
functions by regarding the `SummarizedExperiment` object as a
long-format tibble, `glyexp` splits each `dplyr` verb into two: one for
the row data, the other for the column data.

> **Note:** This vignette assumes you are familiar with both
> `SummarizedExperiment` and `dplyr`.

> **Another Note:** `glyexp` provides two subclasses of
> `SummarizedExperiment`: `GlycomicSE` and `GlycoproteomicSE`. All
> functions introduced in this vignette work with all three of them.

First, let’s create a `SummarizedExperiment` object.

``` r

expr_mat <- matrix(1:24, nrow = 4)
rownames(expr_mat) <- c("V1", "V2", "V3", "V4")
colnames(expr_mat) <- c("S1", "S2", "S3", "S4", "S5", "S6")
col_data <- DataFrame(
  group = c("A", "A", "A", "B", "B", "B"),
  batch = c(1, 2, 1, 2, 1, 2),
  row.names = colnames(expr_mat)
)
row_data <- DataFrame(
  protein = c("PRO1", "PRO2", "PRO3", "PRO3"),
  peptide = c("PEP1", "PEP2", "PEP3", "PEP4"),
  glycan_composition = c("H5N2", "H5N2", "H3N2", "H3N2"),
  row.names = rownames(expr_mat)
)
toy_exp <- SummarizedExperiment(
  assays = SimpleList(data = expr_mat),
  rowData = row_data,
  colData = col_data
)
print(toy_exp)
#> class: SummarizedExperiment 
#> dim: 4 6 
#> metadata(0):
#> assays(1): data
#> rownames(4): V1 V2 V3 V4
#> rowData names(3): protein peptide glycan_composition
#> colnames(6): S1 S2 ... S5 S6
#> colData names(2): group batch
```

## Two Flavors: `_col()` and `_row()`

Every dplyr-style function in glyexp comes in two variants:

- **`_col()` functions**: Work on column data (observation metadata) in
  `SummarizedExperiment` objects
- **`_row()` functions**: Work on row data (variable metadata) in
  `SummarizedExperiment` objects

Both variants automatically update the expression matrix to maintain
synchronization. The mental model should be: “I want to update the row
data, so I should use `xxx_row()` functions like regular `dplyr`
functions, then the expression matrix will update itself.”

Filtering is the most common operation. Say you want to focus only on
group “A” samples:

``` r

# the SummarizedExperiment way
filtered_exp1 <- toy_exp[, toy_exp$group == "A"]

# the glyexp way
filtered_exp2 <- filter_col(toy_exp, group == "A")

# same result
identical(filtered_exp1, filtered_exp2)
#> [1] TRUE
```

In this simple example, there is no clear benefits of using the glyexp
way. The true benefits come with chaining, when you have more than one
operations to do:

``` r

# the SummarizedExperiment way
filtered_exp3 <- toy_exp
colData(filtered_exp3)$batch <- factor(colData(filtered_exp3)$batch)
colData(filtered_exp3)$group <- factor(colData(filtered_exp3)$group)
filtered_exp3 <- filtered_exp3[
  rowData(filtered_exp3)$glycan_composition %in% c("H5N2", "N3N2"),
  filtered_exp3$group == "A"
]

# the glyexp way
filtered_exp4 <- toy_exp |>
  mutate_col(group = factor(group), batch = factor(batch)) |>
  filter_col(group == "A", .drop_levels = FALSE) |>
  filter_row(glycan_composition %in% c("H5N2", "N3N2"))

# the same result
identical(filtered_exp3, filtered_exp4)
#> [1] TRUE
```

Now the merits of `glyexp` is clear: You can use `dplyr`-style data
manipulation directly on `SummarizedExperiment` objects, no matter how
long your operation pipeline is.

## Virtual Index Columns

In a `SummarizedExperiment`, `rowData` and `colData` are `DataFrame`
objects. `DataFrame`s can have row names, which can be accessed by
[`rownames()`](https://rdrr.io/r/base/colnames.html).

``` r

rownames(colData(toy_exp))
#> [1] "S1" "S2" "S3" "S4" "S5" "S6"
rownames(rowData(toy_exp))
#> [1] "V1" "V2" "V3" "V4"
```

These row names match the row or column names of the assay (expression
matrix):

``` r

rownames(assay(toy_exp))
#> [1] "V1" "V2" "V3" "V4"
colnames(assay(toy_exp))
#> [1] "S1" "S2" "S3" "S4" "S5" "S6"
```

In fact, you can get these names by directly calling
[`rownames()`](https://rdrr.io/r/base/colnames.html) or
[`colnames()`](https://rdrr.io/r/base/colnames.html) on the
`SummarizedExperiment` object:

``` r

rownames(toy_exp)
#> [1] "V1" "V2" "V3" "V4"
colnames(toy_exp)
#> [1] "S1" "S2" "S3" "S4" "S5" "S6"
```

`dplyr` functions do not have supports to row names operations, because
they were designed to be used on `tibble`s, which do not have row names.

In `glyexp`, however, we provide a unique techqnue to manipulate the row
names with the `dplyr`-style functions: the **virtual index columns**.

The row names of the row and column data can be referred to as a plain
`.variable` or `.sample` column, respectively.

For example, you can update the row and column names of the
`SummarizedExperiment` by:

``` r

toy_exp |>
  mutate_col(.sample = str_replace(.sample, "S", "Sample_")) |>
  mutate_row(.variable = paste(protein, peptide, glycan_composition, sep = "-"))
#> class: SummarizedExperiment 
#> dim: 4 6 
#> metadata(0):
#> assays(1): data
#> rownames(4): PRO1-PEP1-H5N2 PRO2-PEP2-H5N2 PRO3-PEP3-H3N2
#>   PRO3-PEP4-H3N2
#> rowData names(3): protein peptide glycan_composition
#> colnames(6): Sample_1 Sample_2 ... Sample_5 Sample_6
#> colData names(2): group batch
```

To understand it better, you can regard the row and column data as:

``` r

rownames_to_column(as.data.frame(rowData(toy_exp)), ".variable")
#>   .variable protein peptide glycan_composition
#> 1        V1    PRO1    PEP1               H5N2
#> 2        V2    PRO2    PEP2               H5N2
#> 3        V3    PRO3    PEP3               H3N2
#> 4        V4    PRO3    PEP4               H3N2
rownames_to_column(as.data.frame(colData(toy_exp)), ".sample")
#>   .sample group batch
#> 1      S1     A     1
#> 2      S2     A     2
#> 3      S3     A     1
#> 4      S4     B     2
#> 5      S5     B     1
#> 6      S6     B     2
```

when you use the `dplyr`-style functions.

## Complete Function Reference

glyexp provides dplyr-style equivalents for common data manipulation
functions. Each function comes in both `_col()` and `_row()` variants,
and all automatically maintain matrix synchronization.

### Core Data Manipulation Functions

| Standard dplyr | Sample Operations | Variable Operations | Description |
|:---|:---|:---|:---|
| [`filter()`](https://rdrr.io/r/stats/filter.html) | [`filter_col()`](https://glycoverse.github.io/glyexp/reference/filter_col.md) | [`filter_row()`](https://glycoverse.github.io/glyexp/reference/filter_col.md) | Subset with sync |
| `select()` | [`select_col()`](https://glycoverse.github.io/glyexp/reference/select_col.md) | [`select_row()`](https://glycoverse.github.io/glyexp/reference/select_col.md) | Choose with protection |
| `arrange()` | [`arrange_col()`](https://glycoverse.github.io/glyexp/reference/arrange_col.md) | [`arrange_row()`](https://glycoverse.github.io/glyexp/reference/arrange_col.md) | Sort with order |
| `mutate()` | [`mutate_col()`](https://glycoverse.github.io/glyexp/reference/mutate_col.md) | [`mutate_row()`](https://glycoverse.github.io/glyexp/reference/mutate_col.md) | Create with consistency |
| `rename()` | [`rename_col()`](https://glycoverse.github.io/glyexp/reference/rename_col.md) | [`rename_row()`](https://glycoverse.github.io/glyexp/reference/rename_col.md) | Rename with safety |

### Advanced Slicing Functions

| Standard dplyr | Sample Operations | Variable Operations | Description |
|:---|:---|:---|:---|
| `slice()` | [`slice_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | [`slice_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | Position-based selection |
| `slice_head()` | [`slice_head_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | [`slice_head_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | Top n with sync |
| `slice_tail()` | [`slice_tail_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | [`slice_tail_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | Bottom n with sync |
| `slice_sample()` | [`slice_sample_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | [`slice_sample_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | Random with consistency |
| `slice_max()` | [`slice_max_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | [`slice_max_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | Highest values with order |
| `slice_min()` | [`slice_min_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | [`slice_min_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md) | Lowest values with order |

### Joining Functions

| Standard dplyr | Sample Operations | Variable Operations | Description |
|:---|:---|:---|:---|
| `left_join()` | [`left_join_col()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md) | [`left_join_row()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md) | Add new columns from another table (left join) |
| `inner_join()` | [`inner_join_col()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md) | [`inner_join_row()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md) | Add new columns from another table (inner join) |
| `semi_join()` | [`semi_join_col()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md) | [`semi_join_row()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md) | Filter rows from another table (semi join) |
| `anti_join()` | [`anti_join_col()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md) | [`anti_join_row()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md) | Filter rows from another table (anti join) |

## Function-by-Function Examples

### Selection

``` r

# Select specific columns from sample info
selected_exp <- select_col(toy_exp, group)
colData(selected_exp)
#> DataFrame with 6 rows and 1 column
#>          group
#>    <character>
#> S1           A
#> S2           A
#> S3           A
#> S4           B
#> S5           B
#> S6           B
```

``` r

# Select columns from variable info (notice the index protection!)
var_selected_exp <- select_row(toy_exp, glycan_composition)
rowData(var_selected_exp)
#> DataFrame with 4 rows and 1 column
#>    glycan_composition
#>           <character>
#> V1               H5N2
#> V2               H5N2
#> V3               H3N2
#> V4               H3N2
```

Use `dplyr`-style helpers like `starts_with()`, `ends_with()`, and
`contains()`:

``` r

# Select columns starting with "glycan"
helper_exp <- select_row(toy_exp, starts_with("glycan"))
rowData(helper_exp)
#> DataFrame with 4 rows and 1 column
#>    glycan_composition
#>           <character>
#> V1               H5N2
#> V2               H5N2
#> V3               H3N2
#> V4               H3N2
```

### Arrangement

``` r

# Arrange samples by batch and group
arranged_exp <- arrange_col(toy_exp, batch, group)
rowData(arranged_exp)
#> DataFrame with 4 rows and 3 columns
#>        protein     peptide glycan_composition
#>    <character> <character>        <character>
#> V1        PRO1        PEP1               H5N2
#> V2        PRO2        PEP2               H5N2
#> V3        PRO3        PEP3               H3N2
#> V4        PRO3        PEP4               H3N2
```

Check how the expression matrix columns rearranged to match:

``` r

# Expression matrix columns follow the new sample order
assay(arranged_exp)
#>    S1 S3 S5 S2 S4 S6
#> V1  1  9 17  5 13 21
#> V2  2 10 18  6 14 22
#> V3  3 11 19  7 15 23
#> V4  4 12 20  8 16 24
```

### Mutation

``` r

# Add a new calculated column to sample info
mutated_exp <- mutate_col(
  toy_exp,
  group_batch = paste(group, batch, sep = "_")
)
colData(mutated_exp)
#> DataFrame with 6 rows and 3 columns
#>          group     batch group_batch
#>    <character> <numeric> <character>
#> S1           A         1         A_1
#> S2           A         2         A_2
#> S3           A         1         A_1
#> S4           B         2         B_2
#> S5           B         1         B_1
#> S6           B         2         B_2
```

``` r

# Extract number of Hex and HexNAc
complex_exp <- mutate_row(
  toy_exp,
  nH = as.integer(str_extract(glycan_composition, "H(\\d+)", 1)),
  nN = as.integer(str_extract(glycan_composition, "N(\\d+)", 1)),
)
rowData(complex_exp)
#> DataFrame with 4 rows and 5 columns
#>        protein     peptide glycan_composition        nH        nN
#>    <character> <character>        <character> <integer> <integer>
#> V1        PRO1        PEP1               H5N2         5         2
#> V2        PRO2        PEP2               H5N2         5         2
#> V3        PRO3        PEP3               H3N2         3         2
#> V4        PRO3        PEP4               H3N2         3         2
```

### Slicing

``` r

# Take the first 2 samples
head_exp <- slice_head_col(toy_exp, n = 2)
colData(head_exp)
#> DataFrame with 2 rows and 2 columns
#>          group     batch
#>    <character> <numeric>
#> S1           A         1
#> S2           A         2
```

``` r

# Expression matrix automatically adjusts
assay(head_exp)
#>    S1 S2
#> V1  1  5
#> V2  2  6
#> V3  3  7
#> V4  4  8
```

``` r

# Sample randomly from variables
set.seed(123)  # For reproducibility
random_exp <- slice_sample_row(toy_exp, n = 3)
rowData(random_exp)
#> DataFrame with 3 rows and 3 columns
#>        protein     peptide glycan_composition
#>    <character> <character>        <character>
#> V3        PRO3        PEP3               H3N2
#> V4        PRO3        PEP4               H3N2
#> V1        PRO1        PEP1               H5N2
```

### Renaming

``` r

# Rename columns in sample info
renamed_exp <- rename_col(toy_exp, experimental_group = group)
colData(renamed_exp)
#> DataFrame with 6 rows and 2 columns
#>    experimental_group     batch
#>           <character> <numeric>
#> S1                  A         1
#> S2                  A         2
#> S3                  A         1
#> S4                  B         2
#> S5                  B         1
#> S6                  B         2
```

The index column “sample” remains protected, but everything else can be
renamed freely.

### Joining

These functions can be useful if you have additional information stored
in a separate tibble, and you want to add it to your data container.

``` r

# Join sample info with variable info
more_sample_info <- tibble::tibble(
  .sample = c("S2", "S3", "S4", "S5", "S6"),
  age = c(21, 22, 23, 24, 25),
  gender = c("F", "M", "F", "M", "F")
)
joined_exp <- left_join_col(toy_exp, more_sample_info, by = ".sample")
colData(joined_exp)
#> DataFrame with 6 rows and 4 columns
#>          group     batch       age      gender
#>    <character> <numeric> <numeric> <character>
#> S1           A         1        NA          NA
#> S2           A         2        21           F
#> S3           A         1        22           M
#> S4           B         2        23           F
#> S5           B         1        24           M
#> S6           B         2        25           F
```

You might have noticed that we don’t have alternatives for
[`dplyr::right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
and
[`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html).
This is because by design joining functions in `glyexp` should only be
used to add new information to your data container. However,
`right_join()` and `full_join()` will add more observations to the
resulting tibbles, which is not suitable for `SummarizedExperiment`
objects.

For the same reason, the `relationship` parameter is fixed to
“many-to-one” for all joining functions in `glyexp`. You probably don’t
need to know this, but if you do, check out the documentation of
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
for more details.

## When dplyr-Style Functions Cannot Help

Sometimes you need functionality beyond what glyexp’s dplyr-style
functions provide. Extract the component and use any function you want.

### Why Doesn’t glyexp Implement All dplyr Functions?

glyexp only implements functions that preserve the synchronized
multi-table structure of `SummarizedExperiment` objects.

Functions like `count()`, `distinct()`, `summarise()`, and `pull()`
return aggregated results that break the original data relationships.
For these operations, extract the relevant component and use standard
dplyr functions:

``` r

# For complex aggregations
toy_exp |>
  colData() |>
  as_tibble() |>
  count(group)
#> # A tibble: 2 × 2
#>   group     n
#>   <chr> <int>
#> 1 A         3
#> 2 B         3
```

``` r

# For distinct values
toy_exp |>
  rowData() |>
  as_tibble() |>
  distinct(protein) |>
  pull(protein)
#> [1] "PRO1" "PRO2" "PRO3"
```

``` r

# Or just
unique(rowData(toy_exp)$protein)
#> [1] "PRO1" "PRO2" "PRO3"
```

## Common Pitfalls and How to Avoid Them

### Pitfall 1: Using glyexp Functions on Non-SE Objects

This won’t work:

``` r

regular_tibble <- tibble(group = c("A", "B"), value = c(1, 2))
filter_col(regular_tibble, group == "A")
#> Error in `filter_info_data()`:
#> ! is_tidy_container(exp) is not TRUE
```

### Pitfall 2: Forgetting the Synchronization

Don’t do this:

``` r

# This will not update `toy_exp` at all!
sample_info <- colData(toy_exp) |> as_tibble()
filtered_samples <- filter(sample_info, group == "A")
```

Do this instead:

``` r

filtered_exp <- filter_col(toy_exp, group == "A")
```

### Pitfall 3: Trying to Remove Index Columns

This won’t work as expected:

``` r

select_col(toy_exp, -.sample)
#> Error:
#> ! You should not explicitly select or deselect the ".sample" column in
#>   `sample_info`.
#> ℹ The ".sample" column will be handled by `select_col()` or `select_row()`
#>   automatically.
```

Embrace the protection:

``` r

clean_exp <- select_col(toy_exp, group)
colData(clean_exp)
#> DataFrame with 6 rows and 1 column
#>          group
#>    <character>
#> S1           A
#> S2           A
#> S3           A
#> S4           B
#> S5           B
#> S6           B
```

### Pitfall 4: Mismatched Operations

Don’t mix operations inappropriately:

``` r

# `glycan_cimposition` is a column in the row data
# `arrange_col()` works for column data
arrange_col(toy_exp, glycan_composition)
```

Use the right function for the right data:

``` r

arranged_by_composition <- arrange_row(toy_exp, glycan_composition)
```

## Performance Considerations

glyexp’s dplyr-style functions are designed to be fast, safe, and
consistent.

For large datasets, consider:

- Filtering early in your pipeline to reduce data size
- Using
  [`select_col()`](https://glycoverse.github.io/glyexp/reference/select_col.md)
  and
  [`select_row()`](https://glycoverse.github.io/glyexp/reference/select_col.md)
  to keep only needed columns
- Chaining operations efficiently to minimize intermediate copies

``` r

# Efficient pipeline: filter first, then manipulate
efficient_pipeline <- toy_exp |>
  filter_col(group == "A") |>          # Reduce samples early
  filter_row(protein == "PRO1") |>     # Reduce variables early
  select_col(group) |>                 # Keep only needed sample columns
  select_row(glycan_composition)       # Keep only needed variable columns
```

## Philosophy Behind the Design

glyexp’s dplyr-style functions embody a simple philosophy:

**“Think about your metadata, and let the data follow.”**

This design means:

1.  **Mental Model Alignment**: Think in terms of samples and variables,
    not matrix indices
2.  **Error Prevention**: Automatic synchronization prevents common data
    analysis mistakes
3.  **Familiar Syntax**: If you know dplyr, you already know most of
    glyexp
4.  **Composability**: Functions chain together naturally for complex
    analyses
