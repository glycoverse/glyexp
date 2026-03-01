# dplyr-Style Functions: Data Harmony in Action

This vignette describes glyexp’s dplyr-style functions for synchronized
data manipulation.

When working with multi-table datasets, filtering one table can
desynchronize your data from other components. Rearranging another table
can break carefully established relationships.

**glyexp’s dplyr-style functions** address this by understanding the
connection between your expression matrix, sample information, and
variable annotations. When you transform one component, everything else
follows in synchronization.

**Note:** These functions only work with
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
objects - they cannot be used on regular data.frames, tibbles, or other
data structures.

``` r
library(glyexp)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(conflicted)

conflicts_prefer(glyexp::select_var)
#> [conflicted] Will prefer glyexp::select_var over
#> any other package.
conflicts_prefer(dplyr::filter)
#> [conflicted] Will prefer dplyr::filter over any
#> other package.
```

## Core Philosophy: One Action, Three Updates

glyexp’s dplyr-style functions work on three components:

1.  **Expression Matrix**: Numerical data
2.  **Sample Info**: Experimental metadata
3.  **Variable Info**: Molecular annotations

In traditional data analysis, filtering samples requires manually
updating all related tables. glyexp’s dplyr-style functions handle this
synchronization automatically.

Here’s an example:

``` r
toy_exp <- toy_experiment
print(toy_exp)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
```

## Two Flavors: `_obs()` and `_var()`

Every dplyr-style function in glyexp comes in two variants:

- **`_obs()` functions**: Work on sample information in
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
  objects
- **`_var()` functions**: Work on variable annotations in
  [`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
  objects

Both variants automatically update the expression matrix to maintain
synchronization.

These functions require an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object as input and return an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object as output. For standard tibbles or data.frames, use regular dplyr
functions directly.

### Filtering

Filtering is the most common operation:

#### Sample-Based Filtering with `filter_obs()`

Say you want to focus only on group “A” samples:

``` r
# Before filtering - let's see what we have
get_sample_info(toy_exp)
#> # A tibble: 6 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
#> 4 S4     B         2
#> 5 S5     B         1
#> 6 S6     B         2
```

``` r
# Filter for group A samples only
filtered_exp <- filter_obs(toy_exp, group == "A")
get_sample_info(filtered_exp)
#> # A tibble: 3 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
```

Check the expression matrix:

``` r
# Original matrix dimensions:
dim(get_expr_mat(toy_exp))
#> [1] 4 6

# Original matrix:
get_expr_mat(toy_exp)
#>    S1 S2 S3 S4 S5 S6
#> V1  1  5  9 13 17 21
#> V2  2  6 10 14 18 22
#> V3  3  7 11 15 19 23
#> V4  4  8 12 16 20 24
```

``` r
# Filtered expression matrix - automatically updated!

# Filtered matrix dimensions:
dim(get_expr_mat(filtered_exp))
#> [1] 4 3

# Filtered matrix:
get_expr_mat(filtered_exp)
#>    S1 S2 S3
#> V1  1  5  9
#> V2  2  6 10
#> V3  3  7 11
#> V4  4  8 12
```

The expression matrix is automatically filtered to match the remaining
samples.

#### Variable-Based Filtering with `filter_var()`

Now let’s filter variables and watch the same magic happen:

``` r
# Filter for specific glycan compositions
var_filtered_exp <- filter_var(toy_exp, glycan_composition == "H5N2")
get_var_info(var_filtered_exp)
#> # A tibble: 2 × 4
#>   variable protein peptide glycan_composition
#>   <chr>    <chr>   <chr>   <chr>             
#> 1 V1       PRO1    PEP1    H5N2              
#> 2 V2       PRO2    PEP2    H5N2
```

``` r
# The expression matrix rows automatically follow suit!
get_expr_mat(var_filtered_exp)
#>    S1 S2 S3 S4 S5 S6
#> V1  1  5  9 13 17 21
#> V2  2  6 10 14 18 22
```

**The matrix rows automatically reduced to match the filtered
variables!** This is the core power of glyexp - you think about your
metadata, and the expression data follows your lead.

#### Chaining Filters

Both samples and variables can be filtered by chaining operations:

``` r
double_filtered <- toy_exp |>
  filter_obs(group == "A") |>
  filter_var(glycan_composition %in% c("H5N2", "N3N2"))

# Final dimensions after double filtering:
dim(get_expr_mat(double_filtered))
#> [1] 2 3
get_expr_mat(double_filtered)
#>    S1 S2 S3
#> V1  1  5  9
#> V2  2  6 10
```

The functions support pipe operations:

## Index Columns: Guardians of Data Integrity

Index columns (like “sample” and “variable”) are essential for
maintaining data relationships. Removing them would break
synchronization.

Let’s see this protection in action:

### Attempting to Remove Index Columns

``` r
# Try to select everything EXCEPT the sample index column
protective_exp <- select_obs(toy_exp, -sample)
#> Error:
#> ! You should not explicitly select or deselect the "sample" column in
#>   `sample_info`.
#> ℹ The "sample" column will be handled by `select_obs()` or `select_var()`
#>   automatically.
get_sample_info(protective_exp)
#> Error:
#> ! object 'protective_exp' not found
```

glyexp throws an error to protect data integrity:

``` r
# Same protection for variable info
protective_var_exp <- select_var(toy_exp, -variable)
#> Error:
#> ! You should not explicitly select or deselect the "variable" column in
#>   `var_info`.
#> ℹ The "variable" column will be handled by `select_obs()` or `select_var()`
#>   automatically.
get_var_info(protective_var_exp)
#> Error:
#> ! object 'protective_var_exp' not found
```

Similarly, glyexp throws an error to protect the “variable” column from
being removed.

### Why This Protection Matters

Without index columns, an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object would lose its ability to:

- Keep expression matrix and metadata synchronized
- Validate data consistency
- Enable seamless subsetting operations
- Work with other glycoverse packages

Index columns are essential for maintaining data relationships.

## Complete Function Reference

glyexp provides dplyr-style equivalents for common data manipulation
functions. Each function comes in both `_obs()` and `_var()` variants,
and all automatically maintain matrix synchronization.

These functions are methods specifically for
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
objects.

### Core Data Manipulation Functions

| Standard dplyr                                                    | Sample Operations                                                                   | Variable Operations                                                                 | Description             |
|:------------------------------------------------------------------|:------------------------------------------------------------------------------------|:------------------------------------------------------------------------------------|:------------------------|
| [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)   | [`filter_obs()`](https://glycoverse.github.io/glyexp/dev/reference/filter_obs.md)   | [`filter_var()`](https://glycoverse.github.io/glyexp/dev/reference/filter_obs.md)   | Subset with sync        |
| [`select()`](https://dplyr.tidyverse.org/reference/select.html)   | [`select_obs()`](https://glycoverse.github.io/glyexp/dev/reference/select_obs.md)   | [`select_var()`](https://glycoverse.github.io/glyexp/dev/reference/select_obs.md)   | Choose with protection  |
| [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html) | [`arrange_obs()`](https://glycoverse.github.io/glyexp/dev/reference/arrange_obs.md) | [`arrange_var()`](https://glycoverse.github.io/glyexp/dev/reference/arrange_obs.md) | Sort with order         |
| [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)   | [`mutate_obs()`](https://glycoverse.github.io/glyexp/dev/reference/mutate_obs.md)   | [`mutate_var()`](https://glycoverse.github.io/glyexp/dev/reference/mutate_obs.md)   | Create with consistency |
| [`rename()`](https://dplyr.tidyverse.org/reference/rename.html)   | [`rename_obs()`](https://glycoverse.github.io/glyexp/dev/reference/rename_obs.md)   | [`rename_var()`](https://glycoverse.github.io/glyexp/dev/reference/rename_obs.md)   | Rename with safety      |

### Advanced Slicing Functions

| Standard dplyr                                                       | Sample Operations                                                                      | Variable Operations                                                                    | Description               |
|:---------------------------------------------------------------------|:---------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------|:--------------------------|
| [`slice()`](https://dplyr.tidyverse.org/reference/slice.html)        | [`slice_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)        | [`slice_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)        | Position-based selection  |
| [`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html)   | [`slice_head_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)   | [`slice_head_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)   | Top n with sync           |
| [`slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html)   | [`slice_tail_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)   | [`slice_tail_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)   | Bottom n with sync        |
| [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html) | [`slice_sample_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md) | [`slice_sample_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md) | Random with consistency   |
| [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html)    | [`slice_max_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)    | [`slice_max_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)    | Highest values with order |
| [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html)    | [`slice_min_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)    | [`slice_min_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)    | Lowest values with order  |

### Joining Functions

| Standard dplyr                                                            | Sample Operations                                                                        | Variable Operations                                                                      | Description                                     |
|:--------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------|:------------------------------------------------|
| [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)  | [`left_join_obs()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)  | [`left_join_var()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)  | Add new columns from another table (left join)  |
| [`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html) | [`inner_join_obs()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md) | [`inner_join_var()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md) | Add new columns from another table (inner join) |
| [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)  | [`semi_join_obs()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)  | [`semi_join_var()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)  | Filter rows from another table (semi join)      |
| [`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)  | [`anti_join_obs()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)  | [`anti_join_var()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)  | Filter rows from another table (anti join)      |

## Function-by-Function Examples

### Selection

``` r
# Select specific columns from sample info
selected_exp <- select_obs(toy_exp, group, batch)
get_sample_info(selected_exp)
#> # A tibble: 6 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
#> 4 S4     B         2
#> 5 S5     B         1
#> 6 S6     B         2
```

``` r
# Select columns from variable info (notice the index protection!)
var_selected_exp <- select_var(toy_exp, glycan_composition)
get_var_info(var_selected_exp)
#> # A tibble: 4 × 2
#>   variable glycan_composition
#>   <chr>    <chr>             
#> 1 V1       H5N2              
#> 2 V2       H5N2              
#> 3 V3       H3N2              
#> 4 V4       H3N2
```

Use `dplyr`-style helpers like
[`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
and
[`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html):

``` r
# Select columns starting with "glycan"
helper_exp <- select_var(toy_exp, starts_with("glycan"))
get_var_info(helper_exp)
#> # A tibble: 4 × 2
#>   variable glycan_composition
#>   <chr>    <chr>             
#> 1 V1       H5N2              
#> 2 V2       H5N2              
#> 3 V3       H3N2              
#> 4 V4       H3N2
```

### Arrangement

``` r
# Arrange samples by batch and group
arranged_exp <- arrange_obs(toy_exp, batch, group)
get_sample_info(arranged_exp)
#> # A tibble: 6 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S3     A         1
#> 3 S5     B         1
#> 4 S2     A         2
#> 5 S4     B         2
#> 6 S6     B         2
```

Check how the expression matrix columns rearranged to match:

``` r
# Expression matrix columns follow the new sample order
get_expr_mat(arranged_exp)
#>    S1 S3 S5 S2 S4 S6
#> V1  1  9 17  5 13 21
#> V2  2 10 18  6 14 22
#> V3  3 11 19  7 15 23
#> V4  4 12 20  8 16 24
```

### Mutation

``` r
# Add a new calculated column to sample info
mutated_exp <- mutate_obs(
  toy_exp,
  group_batch = paste(group, batch, sep = "_")
)
get_sample_info(mutated_exp)
#> # A tibble: 6 × 4
#>   sample group batch group_batch
#>   <chr>  <chr> <dbl> <chr>      
#> 1 S1     A         1 A_1        
#> 2 S2     A         2 A_2        
#> 3 S3     A         1 A_1        
#> 4 S4     B         2 B_2        
#> 5 S5     B         1 B_1        
#> 6 S6     B         2 B_2
```

``` r
# Create a complexity score for variables
complex_exp <- mutate_var(
  toy_exp,
  complexity = nchar(glycan_composition)
)
get_var_info(complex_exp)
#> # A tibble: 4 × 5
#>   variable protein peptide glycan_composition complexity
#>   <chr>    <chr>   <chr>   <chr>                   <int>
#> 1 V1       PRO1    PEP1    H5N2                        4
#> 2 V2       PRO2    PEP2    H5N2                        4
#> 3 V3       PRO3    PEP3    H3N2                        4
#> 4 V4       PRO3    PEP4    H3N2                        4
```

### Slicing

``` r
# Take the first 2 samples
head_exp <- slice_head_obs(toy_exp, n = 2)
get_sample_info(head_exp)
#> # A tibble: 2 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
```

``` r
# Expression matrix automatically adjusts
get_expr_mat(head_exp)
#>    S1 S2
#> V1  1  5
#> V2  2  6
#> V3  3  7
#> V4  4  8
```

``` r
# Sample randomly from variables
set.seed(123)  # For reproducibility
random_exp <- slice_sample_var(toy_exp, n = 3)
get_var_info(random_exp)
#> # A tibble: 3 × 4
#>   variable protein peptide glycan_composition
#>   <chr>    <chr>   <chr>   <chr>             
#> 1 V3       PRO3    PEP3    H3N2              
#> 2 V4       PRO3    PEP4    H3N2              
#> 3 V1       PRO1    PEP1    H5N2
```

### Renaming

``` r
# Rename columns in sample info
renamed_exp <- rename_obs(toy_exp, experimental_group = group)
get_sample_info(renamed_exp)
#> # A tibble: 6 × 3
#>   sample experimental_group batch
#>   <chr>  <chr>              <dbl>
#> 1 S1     A                      1
#> 2 S2     A                      2
#> 3 S3     A                      1
#> 4 S4     B                      2
#> 5 S5     B                      1
#> 6 S6     B                      2
```

The index column “sample” remains protected, but everything else can be
renamed freely.

### Joining

These functions can be useful if you have additional information stored
in a separate tibble, and you want to add it to your
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object.

``` r
# Join sample info with variable info
more_sample_info <- tibble::tibble(
  sample = c("S1", "S2", "S3", "S4", "S5", "S6"),
  age = c(20, 21, 22, 23, 24, 25),
  gender = c("M", "F", "M", "F", "M", "F")
)
joined_exp <- left_join_obs(toy_exp, more_sample_info, by = "sample")
get_sample_info(joined_exp)
#> # A tibble: 6 × 5
#>   sample group batch   age gender
#>   <chr>  <chr> <dbl> <dbl> <chr> 
#> 1 S1     A         1    20 M     
#> 2 S2     A         2    21 F     
#> 3 S3     A         1    22 M     
#> 4 S4     B         2    23 F     
#> 5 S5     B         1    24 M     
#> 6 S6     B         2    25 F
```

You might have noticed that we don’t have alternatives for
[`dplyr::right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
and
[`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html).
This is because by design joining functions in `glyexp` should only be
used to add new information to your
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object. However,
[`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
and
[`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
will add more observations to the resulting tibbles, which is not
suitable for
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
objects.

For the same reason, the `relationship` parameter is fixed to
“many-to-one” for all joining functions in `glyexp`. You probably don’t
need to know this, but if you do, check out the documentation of
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
for more details.

## Advanced Patterns: Chaining for Complex Operations

The real power emerges when you chain multiple operations together. Here
are some patterns:

### Pattern 1: Filter → Select → Arrange

``` r
complex_pipeline <- toy_exp |>
  filter_obs(group == "A") |>
  select_obs(group, batch) |>
  arrange_obs(desc(batch)) |>
  filter_var(protein == "PRO1") |>
  select_var(glycan_composition, protein)

print("Final pipeline result:")
#> [1] "Final pipeline result:"
print(complex_pipeline)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 1 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: glycan_composition <chr>, protein <chr>
```

### Pattern 2: Mutate → Filter → Slice

``` r
analytical_pipeline <- toy_exp |>
  mutate_var(composition_length = nchar(glycan_composition)) |>
  filter_var(composition_length >= 4) |>
  slice_max_var(composition_length, n = 3)

get_var_info(analytical_pipeline)
#> # A tibble: 4 × 5
#>   variable protein peptide glycan_composition composition_length
#>   <chr>    <chr>   <chr>   <chr>                           <int>
#> 1 V1       PRO1    PEP1    H5N2                                4
#> 2 V2       PRO2    PEP2    H5N2                                4
#> 3 V3       PRO3    PEP3    H3N2                                4
#> 4 V4       PRO3    PEP4    H3N2                                4
```

### Pattern 3: Random Sampling for Testing

``` r
# Create a smaller dataset for testing
set.seed(456)
test_exp <- toy_exp |>
  slice_sample_obs(n = 3) |>
  slice_sample_var(n = 4)

print("Test dataset dimensions:")
#> [1] "Test dataset dimensions:"
print(test_exp)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
```

## When dplyr-Style Functions Cannot Help

Sometimes you need functionality beyond what glyexp’s dplyr-style
functions provide. Extract the tibbles and use any dplyr function you
want.

### Why Doesn’t glyexp Implement All dplyr Functions?

glyexp only implements functions that preserve the synchronized
multi-table structure of
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
objects.

Functions like
[`count()`](https://dplyr.tidyverse.org/reference/count.html),
[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html),
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
and [`pull()`](https://dplyr.tidyverse.org/reference/pull.html) return
aggregated results that break the original data relationships. For these
operations, extract the relevant tibble and use standard dplyr
functions:

``` r
# For complex aggregations
toy_exp |>
  get_sample_info() |>
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
  get_var_info() |>
  distinct(protein) |>
  pull(protein)
#> [1] "PRO1" "PRO2" "PRO3"
```

``` r
# For advanced filtering with multiple conditions
complex_filter_conditions <- toy_exp |>
  get_sample_info() |>
  filter(group == "A", batch == 2) |>
  pull(sample)

# Then use the results to subset your experiment
filtered_by_complex <- filter_obs(toy_exp, sample %in% complex_filter_conditions)
```

## Common Pitfalls and How to Avoid Them

### Pitfall 1: Using glyexp Functions on Non-Experiment Objects

This won’t work:

``` r
library(tibble)
regular_tibble <- tibble(group = c("A", "B"), value = c(1, 2))
filter_obs(regular_tibble, group == "A")
#> Error in `filter_info_data()`:
#> ! is_experiment(exp) is not TRUE
```

Do this instead:

``` r
regular_tibble <- tibble(group = c("A", "B"), value = c(1, 2))
filter(regular_tibble, group == "A")
#> # A tibble: 1 × 2
#>   group value
#>   <chr> <dbl>
#> 1 A         1

filtered_exp <- filter_obs(toy_exp, group == "A")
get_sample_info(filtered_exp)
#> # A tibble: 3 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
```

### Pitfall 2: Forgetting the Synchronization

Don’t do this:

``` r
sample_info <- get_sample_info(toy_exp)
filtered_samples <- filter(sample_info, group == "A")
```

Do this instead:

``` r
filtered_exp <- filter_obs(toy_exp, group == "A")
```

### Pitfall 3: Trying to Remove Index Columns

This won’t work as expected:

``` r
select_obs(toy_exp, -sample)
#> Error:
#> ! You should not explicitly select or deselect the "sample" column in
#>   `sample_info`.
#> ℹ The "sample" column will be handled by `select_obs()` or `select_var()`
#>   automatically.
```

Embrace the protection:

``` r
clean_exp <- select_obs(toy_exp, group, batch)
get_sample_info(clean_exp)
#> # A tibble: 6 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
#> 4 S4     B         2
#> 5 S5     B         1
#> 6 S6     B         2
```

### Pitfall 4: Mismatched Operations

Don’t mix operations inappropriately:

``` r
arrange_obs(toy_exp, glycan_composition)
```

Use the right function for the right data:

``` r
arranged_by_composition <- arrange_var(toy_exp, glycan_composition)
get_var_info(arranged_by_composition)
#> # A tibble: 4 × 4
#>   variable protein peptide glycan_composition
#>   <chr>    <chr>   <chr>   <chr>             
#> 1 V3       PRO3    PEP3    H3N2              
#> 2 V4       PRO3    PEP4    H3N2              
#> 3 V1       PRO1    PEP1    H5N2              
#> 4 V2       PRO2    PEP2    H5N2
```

## Performance Considerations

glyexp’s dplyr-style functions are designed to be fast, safe, and
consistent.

For large datasets, consider:

- Filtering early in your pipeline to reduce data size
- Using
  [`select_obs()`](https://glycoverse.github.io/glyexp/dev/reference/select_obs.md)
  and
  [`select_var()`](https://glycoverse.github.io/glyexp/dev/reference/select_obs.md)
  to keep only needed columns
- Chaining operations efficiently to minimize intermediate copies

``` r
# Efficient pipeline: filter first, then manipulate
efficient_pipeline <- toy_exp |>
  filter_obs(group == "A") |>          # Reduce samples early
  filter_var(protein == "PRO1") |>     # Reduce variables early
  select_obs(group) |>                 # Keep only needed sample columns
  select_var(glycan_composition)       # Keep only needed variable columns
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

## Summary

glyexp’s dplyr-style functions are experiment-specific data manipulators
designed exclusively for
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
objects. They provide:

- **Automatic Synchronization**: Operations on metadata automatically
  update the expression matrix
- **Index Column Protection**: Critical relationship columns are
  protected from deletion
- **Familiar Syntax**: Standard dplyr operations with multi-table
  awareness
- **Type-Aware Operations**: `_obs()` for samples, `_var()` for
  variables

Start with
[`filter_obs()`](https://glycoverse.github.io/glyexp/dev/reference/filter_obs.md)
and
[`select_var()`](https://glycoverse.github.io/glyexp/dev/reference/select_obs.md),
then build complex pipelines.
