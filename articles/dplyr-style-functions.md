# dplyr-Style Functions: Data Harmony in Action

Welcome to the world of **synchronized data manipulation**! 🎼

If you’ve ever worked with multi-table datasets, you know the pain:
filter one table, and suddenly your data is out of sync. Rearrange
another, and your carefully crafted relationships crumble like a house
of cards.

**Enter glyexp’s dplyr-style functions** - your new data harmony
conductors! 🎯

These aren’t just regular dplyr functions with a fancy wrapper. They’re
**relationship-aware data manipulators** specifically designed for
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects that understand the intricate dance between your expression
matrix, sample information, and variable annotations. When you transform
one piece, everything else follows in perfect synchronization.

**🎯 Important Note:** These functions **only work with
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects** - you cannot use them on regular data.frames, tibbles, or
other data structures. They are purpose-built for the synchronized data
model that
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
provides.

``` r
library(glyexp)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following object is masked from 'package:glyexp':
#> 
#>     select_var
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

## The Core Philosophy: One Action, Three Updates 🎭

Imagine you’re the conductor of a three-piece orchestra:

🎼 **First violin (Expression Matrix)**: Your numerical data  
🎼 **Second violin (Sample Info)**: Your experimental metadata  
🎼 **Viola (Variable Info)**: Your molecular annotations

In traditional data analysis, when you want the first violin to play a
solo (filter samples), you have to manually cue each instrument. Miss a
beat, and your symphony turns into chaos.

**glyexp’s dplyr-style functions are different.** They’re like having a
magical conductor’s baton - wave it once, and all three instruments
respond in perfect harmony!

Let’s see this magic in action:

``` r
toy_exp <- toy_experiment
print(toy_exp)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
```

## The Two Flavors: `_obs()` and `_var()` 🍦

Every dplyr-style function in glyexp comes in two delicious flavors:

- **`_obs()` functions**: Work on sample information
  (observations/columns) in
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  objects
- **`_var()` functions**: Work on variable annotations (variables/rows)
  in
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  objects

But here’s the beautiful part - **both flavors automatically update the
expression matrix** to maintain perfect synchronization!

**⚠️ Reminder:** These specialized functions require an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object as input and return an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object as output. They cannot be used with standard tibbles or
data.frames - for those, use the regular dplyr functions directly.

### Filtering: The Art of Selective Attention 🔍

Let’s start with the most common operation - filtering your data.

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

**Beautiful!** But here’s where the magic happens - check the expression
matrix:

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

🎪 **Ta-da!** The expression matrix automatically filtered its columns
to match the remaining samples! No manual intervention, no risk of
mismatched data - just pure, synchronized harmony.

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

#### Chaining Filters: The Symphony Continues 🎵

Want to filter both samples and variables? Chain them together like a
beautiful melody:

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

**Notice the pipe-friendly design?** That’s the dplyr DNA in action -
familiar syntax, powerful results!

## The Sacred Index Columns: Guardians of Data Integrity 🛡️

Here’s where glyexp really shines: **index column protection**. These
special columns (like “sample” and “variable”) are the backbone of your
data relationships. Lose them, and your carefully orchestrated data
symphony falls apart.

Let’s see this protection in action:

### Attempting to Remove Index Columns (Spoiler: It Won’t Work!) 😄

``` r
# Try to select everything EXCEPT the sample index column
protective_exp <- select_obs(toy_exp, -sample)
#> Error:
#> ! You should not explicitly select or deselect the "sample" column in
#>   `sample_info`.
#> ℹ The "sample" column will be handled by `select_obs()` or `select_var()`
#>   automatically.
get_sample_info(protective_exp)
#> Error: object 'protective_exp' not found
```

**Did you see that error message?** glyexp throws a helpful error
message and protects our data integrity by preventing this operation
entirely!

``` r
# Same protection for variable info
protective_var_exp <- select_var(toy_exp, -variable)
#> Error:
#> ! You should not explicitly select or deselect the "variable" column in
#>   `var_info`.
#> ℹ The "variable" column will be handled by `select_obs()` or `select_var()`
#>   automatically.
get_var_info(protective_var_exp)
#> Error: object 'protective_var_exp' not found
```

Similarly, glyexp throws an error to protect the “variable” column from
being removed! 🏰

### Why This Protection Matters

Without index columns, your
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object would lose its ability to:

- ✅ Keep expression matrix and metadata synchronized
- ✅ Validate data consistency
- ✅ Enable seamless subsetting operations
- ✅ Work with other `glycoverse` packages

Think of index columns as the **GPS coordinates** of your data - remove
them, and you’re lost in a sea of unconnected numbers!

## The Complete Function Family Tree 🌳

glyexp provides dplyr-style equivalents for all your favorite data
manipulation functions. **Each function comes in both `_obs()` and
`_var()` flavors**, and **all automatically maintain matrix
synchronization**.

**🔧 Technical Note:** All these functions are **methods specifically
for
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects**. Unlike generic dplyr functions that work on various data
types, these functions expect and return
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects exclusively:

### Core Data Manipulation Functions

| Standard dplyr                                                    | Sample Operations                                                               | Variable Operations                                                             | Magic Power                |
|:------------------------------------------------------------------|:--------------------------------------------------------------------------------|:--------------------------------------------------------------------------------|:---------------------------|
| [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)   | [`filter_obs()`](https://glycoverse.github.io/glyexp/reference/filter_obs.md)   | [`filter_var()`](https://glycoverse.github.io/glyexp/reference/filter_obs.md)   | 🔍 Subset with sync        |
| [`select()`](https://dplyr.tidyverse.org/reference/select.html)   | [`select_obs()`](https://glycoverse.github.io/glyexp/reference/select_obs.md)   | [`select_var()`](https://glycoverse.github.io/glyexp/reference/select_obs.md)   | 🎯 Choose with protection  |
| [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html) | [`arrange_obs()`](https://glycoverse.github.io/glyexp/reference/arrange_obs.md) | [`arrange_var()`](https://glycoverse.github.io/glyexp/reference/arrange_obs.md) | 📊 Sort with order         |
| [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)   | [`mutate_obs()`](https://glycoverse.github.io/glyexp/reference/mutate_obs.md)   | [`mutate_var()`](https://glycoverse.github.io/glyexp/reference/mutate_obs.md)   | ➕ Create with consistency |
| [`rename()`](https://dplyr.tidyverse.org/reference/rename.html)   | [`rename_obs()`](https://glycoverse.github.io/glyexp/reference/rename_obs.md)   | [`rename_var()`](https://glycoverse.github.io/glyexp/reference/rename_obs.md)   | 🏷️ Rename with safety      |

### Advanced Slicing Functions

| Standard dplyr                                                       | Sample Operations                                                                  | Variable Operations                                                                | Specialty                    |
|:---------------------------------------------------------------------|:-----------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------|:-----------------------------|
| [`slice()`](https://dplyr.tidyverse.org/reference/slice.html)        | [`slice_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)        | [`slice_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)        | 🔢 Position-based selection  |
| [`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html)   | [`slice_head_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)   | [`slice_head_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)   | ⬆️ Top n with sync           |
| [`slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html)   | [`slice_tail_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)   | [`slice_tail_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)   | ⬇️ Bottom n with sync        |
| [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html) | [`slice_sample_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md) | [`slice_sample_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md) | 🎲 Random with consistency   |
| [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html)    | [`slice_max_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)    | [`slice_max_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)    | 📈 Highest values with order |
| [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html)    | [`slice_min_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)    | [`slice_min_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)    | 📉 Lowest values with order  |

### Joining Functions

| Standard dplyr                                                            | Sample Operations                                                                    | Variable Operations                                                                  | Magic Power                                     |
|:--------------------------------------------------------------------------|:-------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------|:------------------------------------------------|
| [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)  | [`left_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | [`left_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | Add new columns from another table (left join)  |
| [`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html) | [`inner_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md) | [`inner_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md) | Add new columns from another table (inner join) |
| [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)  | [`semi_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | [`semi_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | Filter rows from another table (semi join)      |
| [`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)  | [`anti_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | [`anti_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | Filter rows from another table (anti join)      |

## Deep Dive: Function-by-Function Examples 🏊‍♂️

Let’s explore each function family with hands-on examples!

### Selection: Choosing Your Data Wisely 🎯

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

**Pro tip:** Use `dplyr`-style helpers like
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

### Arrangement: Putting Things in Order 📊

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

**The magic moment:** Check how the expression matrix columns rearranged
to match!

``` r
# Expression matrix columns follow the new sample order
get_expr_mat(arranged_exp)
#>    S1 S3 S5 S2 S4 S6
#> V1  1  9 17  5 13 21
#> V2  2 10 18  6 14 22
#> V3  3 11 19  7 15 23
#> V4  4 12 20  8 16 24
```

### Mutation: Creating New Insights ➕

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

### Slicing: Precision Subsetting 🔢

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

### Renaming: Clarity Through Better Names 🏷️

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

**Notice:** The index column “sample” remains untouchable, but
everything else can be renamed freely!

### Joining: Bridging Tables Together 🔗

These functions can be useful if you have additional information stored
in a separate tibble, and you want to add it to your
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
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
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object. However,
[`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
and
[`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
will add more observations to the resulting tibbles, which is not
suitable for
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects.

For the same reason, the `relationship` parameter is fixed to
“many-to-one” for all joining functions in `glyexp`. You probably don’t
need to know this, but if you do, check out the documentation of
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
for more details.

## Advanced Patterns: Chaining for Complex Operations 🔗

The real power emerges when you chain multiple operations together. Here
are some advanced patterns:

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

## When dplyr-Style Functions Can’t Help: The Escape Hatch 🚪

Sometimes you need functionality that goes beyond what glyexp’s
dplyr-style functions provide. **No problem!** Since glyexp’s
dplyr-style functions **only work with
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects**, when you need standard dplyr functionality, simply extract
the tibbles and use any dplyr function you want.

### Why Doesn’t glyexp Implement All dplyr Functions? 🤔

**The philosophy is simple:** glyexp only implements functions that
**preserve the synchronized multi-table structure** of
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
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

## Common Pitfalls and How to Avoid Them ⚠️

### Pitfall 1: Using glyexp Functions on Non-Experiment Objects

**❌ This won’t work:**

``` r
# glyexp functions only work on experiment() objects!
library(tibble)
regular_tibble <- tibble(group = c("A", "B"), value = c(1, 2))
filter_obs(regular_tibble, group == "A")  # Error: not an experiment object!
#> Error in filter_info_data(exp = exp, info_field = "sample_info", id_column = "sample", : is_experiment(exp) is not TRUE
```

**✅ Do this instead:**

``` r
# Use regular dplyr functions for regular data structures
regular_tibble <- tibble(group = c("A", "B"), value = c(1, 2))
filter(regular_tibble, group == "A")  # Works perfectly!
#> # A tibble: 1 × 2
#>   group value
#>   <chr> <dbl>
#> 1 A         1

# Use glyexp functions only with experiment objects
filtered_exp <- filter_obs(toy_exp, group == "A")  # This works!
get_sample_info(filtered_exp)
#> # A tibble: 3 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
```

### Pitfall 2: Forgetting the Synchronization Magic

**❌ Don’t do this:**

``` r
# This breaks synchronization!
sample_info <- get_sample_info(toy_exp)
filtered_samples <- filter(sample_info, group == "A")
# Now you have filtered sample info but the original expression matrix!
```

**✅ Do this instead:**

``` r
# This maintains synchronization
filtered_exp <- filter_obs(toy_exp, group == "A")
# Everything stays in sync!
```

### Pitfall 3: Trying to Remove Index Columns

**❌ This won’t work as expected:**

``` r
# Index column protection prevents this - will throw an error!
select_obs(toy_exp, -sample)  
#> Error:
#> ! You should not explicitly select or deselect the "sample" column in
#>   `sample_info`.
#> ℹ The "sample" column will be handled by `select_obs()` or `select_var()`
#>   automatically.
```

**✅ Embrace the protection:**

``` r
# Select the columns you want, let glyexp protect the index
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
# "sample" column automatically included for data integrity
```

### Pitfall 4: Mismatched Operations

**❌ Don’t mix operations inappropriately:**

``` r
# This doesn't make sense - you can't arrange sample info by variable properties
arrange_obs(toy_exp, glycan_composition)  # glycan_composition is in var_info!
```

**✅ Use the right function for the right data:**

``` r
# Arrange variables by their glycan composition
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

## Performance Considerations: Speed Meets Safety 🏃‍♂️💨

glyexp’s dplyr-style functions are designed to be:

**🚀 Fast**: Built on top of highly optimized dplyr functions  
**🛡️ Safe**: Index column protection prevents data corruption  
**🔄 Consistent**: Automatic synchronization eliminates manual errors

For large datasets, consider:

- Filtering early in your pipeline to reduce data size
- Using
  [`select_obs()`](https://glycoverse.github.io/glyexp/reference/select_obs.md)
  and
  [`select_var()`](https://glycoverse.github.io/glyexp/reference/select_obs.md)
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

## The Philosophy Behind the Design 🧠

glyexp’s dplyr-style functions embody a simple but powerful philosophy:

**“Think about your metadata, and let the data follow.”** 🎯

This design choice means:

1.  **Mental Model Alignment**: You think in terms of samples and
    variables, not matrix indices
2.  **Error Prevention**: Automatic synchronization prevents the most
    common data analysis mistakes
3.  **Familiar Syntax**: If you know dplyr, you already know 90% of
    glyexp
4.  **Composability**: Functions chain together naturally for complex
    analyses

## Summary 🎯

glyexp’s dplyr-style functions are **experiment-specific data
manipulators** designed exclusively for
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects. They provide four key capabilities:

🎼 **Automatic Synchronization** - Operations on metadata automatically
update the expression matrix  
🛡️ **Index Column Protection** - Critical relationship columns are
protected from deletion  
🔗 **Familiar Syntax** - Standard dplyr operations with multi-table
awareness  
🎯 **Type-Aware Operations** - `_obs()` for samples, `_var()` for
variables

**Start simple with
[`filter_obs()`](https://glycoverse.github.io/glyexp/reference/filter_obs.md)
and
[`select_var()`](https://glycoverse.github.io/glyexp/reference/select_obs.md),
then build complex pipelines!** 🎵
