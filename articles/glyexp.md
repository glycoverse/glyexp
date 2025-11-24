# Get Started with glyexp

Picture this: you’re knee-deep in omics experiments (especially the
fascinating world of glycomics and glycoproteomics), and you’re juggling
three types of data like a lab virtuoso:

1.  **Expression data** - the actual measurements of your biological
    molecules (glycans, glycopeptides, and their friends)
2.  **Molecular annotations** - the ID cards for your molecules
    (structures, sequences, you name it)
3.  **Experimental metadata** - the story behind your samples (time
    points, treatments, experimental conditions)

Here’s where `glyexp` swoops in to save the day! 🦸‍♀️

The
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
class is your new best friend - think of it as a smart container that
keeps all three data types organized and talking to each other. No more
scattered spreadsheets or lost annotations!

**Why should you care?** Every package in the `glycoverse` ecosystem
speaks
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
fluently. It’s like having a universal translator for your glycomics
workflow - everything just *clicks* together.

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

# Resolve function conflicts - prefer glyexp version over deprecated dplyr version
# `dplyr::select_var` is deprecated anyway, so we can safely override it
conflicts_prefer(glyexp::select_var)
#> [conflicted] Will prefer glyexp::select_var over
#> any other package.
```

## Your First Steps into the Glycoverse

Let’s dive in with our trusty toy experiment - think of it as your
training wheels before you tackle the real deal.

``` r
toy_exp <- toy_experiment
toy_exp
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
```

Look at that beautiful summary! When you print an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object, it’s like getting a snapshot of your entire experimental world -
variables, observations, and all the metadata that makes your data
meaningful.

Now, let’s peek under the hood. You can extract the three core
components faster than you can say “glycosylation”:

### 🧬 The Expression Matrix - Your Data’s Heart and Soul

``` r
get_expr_mat(toy_exp)
#>    S1 S2 S3 S4 S5 S6
#> V1  1  5  9 13 17 21
#> V2  2  6 10 14 18 22
#> V3  3  7 11 15 19 23
#> V4  4  8 12 16 20 24
```

This matrix is where the magic happens - rows are your variables
(molecules), columns are your observations (samples), and the numbers
tell your biological story.

### 🏷️ Variable Information - Meet Your Molecules

``` r
get_var_info(toy_exp)
#> # A tibble: 4 × 4
#>   variable protein peptide glycan_composition
#>   <chr>    <chr>   <chr>   <chr>             
#> 1 V1       PRO1    PEP1    H5N2              
#> 2 V2       PRO2    PEP2    H5N2              
#> 3 V3       PRO3    PEP3    H3N2              
#> 4 V4       PRO3    PEP4    H3N2
```

Think of this as your molecular address book - every variable gets its
own detailed profile.

### 📋 Sample Information - Know Your Experiments

``` r
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

And this? This is your experimental diary - tracking every condition,
timepoint, and treatment.

**Here’s the cool part:** Notice how the “variable” column in
[`get_var_info()`](https://glycoverse.github.io/glyexp/reference/get_var_info.md)
and the “sample” column in
[`get_sample_info()`](https://glycoverse.github.io/glyexp/reference/get_sample_info.md)
perfectly match the row and column names in your expression matrix?
That’s no accident!

These are the **index columns** - the secret sauce that keeps everything
synchronized. They’re like the GPS coordinates that ensure your data
stays connected no matter what transformations you throw at it.

## Data Wrangling Made Easy - dplyr Meets glyexp

If you’ve ever used `dplyr` (and who hasn’t?), you’re already 90% of the
way there! 🎉

For every `dplyr` function you know and love, `glyexp` gives you two
specialized versions:

- **`_obs()`** functions: work on your sample metadata
- **`_var()`** functions: work on your variable annotations

Let’s see this in action. Want to focus on just group “A” samples?

``` r
subset_exp <- filter_obs(toy_exp, group == "A")
```

Let’s check what happened to our sample info:

``` r
get_sample_info(subset_exp)
#> # A tibble: 3 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
```

Beautiful! But here’s where the magic really shines - check out the
expression matrix:

``` r
get_expr_mat(subset_exp)
#>    S1 S2 S3
#> V1  1  5  9
#> V2  2  6 10
#> V3  3  7 11
#> V4  4  8 12
```

🎪 **Ta-da!** The expression matrix automatically filtered itself to
match! It’s like having a well-trained assistant who anticipates your
every move.

This is
[`filter_obs()`](https://glycoverse.github.io/glyexp/reference/filter_obs.md)
in a nutshell: “Hey, filter my sample info this way, and oh yeah, make
sure everything else follows suit.” And it does, flawlessly.

Variable filtering works the same way:

``` r
toy_exp |>
  filter_obs(group == "A") |>
  filter_var(glycan_composition == "H5N2") |>
  get_expr_mat()
#>    S1 S2 S3
#> V1  1  5  9
#> V2  2  6 10
```

Notice how these functions support the pipe operator (`|>`)? That’s the
`dplyr` DNA in action!

The pattern is simple: `glyexp` functions are just like their `dplyr`
cousins, but with two superpowers:

1.  They expect and return
    [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
    objects (keeping your data ecosystem intact)
2.  They treat those index columns like precious cargo (no accidental
    deletions here!)

## Complete dplyr Function Reference

Here’s your complete toolkit of supported dplyr-style functions. **These
functions orchestrate seamless coordination between all three data
types** - expression matrix, sample information, and variable
information - ensuring everything stays perfectly synchronized:

| dplyr Function                                                            | For Samples (`_obs`)                                                                 | For Variables (`_var`)                                                               | What It Does                                    |
|:--------------------------------------------------------------------------|:-------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------|:------------------------------------------------|
| [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)           | [`filter_obs()`](https://glycoverse.github.io/glyexp/reference/filter_obs.md)        | [`filter_var()`](https://glycoverse.github.io/glyexp/reference/filter_obs.md)        | Subset rows based on conditions                 |
| [`select()`](https://dplyr.tidyverse.org/reference/select.html)           | [`select_obs()`](https://glycoverse.github.io/glyexp/reference/select_obs.md)        | [`select_var()`](https://glycoverse.github.io/glyexp/reference/select_obs.md)        | Choose specific columns                         |
| [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)         | [`arrange_obs()`](https://glycoverse.github.io/glyexp/reference/arrange_obs.md)      | [`arrange_var()`](https://glycoverse.github.io/glyexp/reference/arrange_obs.md)      | Reorder rows by column values                   |
| [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)           | [`mutate_obs()`](https://glycoverse.github.io/glyexp/reference/mutate_obs.md)        | [`mutate_var()`](https://glycoverse.github.io/glyexp/reference/mutate_obs.md)        | Create/modify columns                           |
| [`rename()`](https://dplyr.tidyverse.org/reference/rename.html)           | [`rename_obs()`](https://glycoverse.github.io/glyexp/reference/rename_obs.md)        | [`rename_var()`](https://glycoverse.github.io/glyexp/reference/rename_obs.md)        | Rename columns                                  |
| [`slice()`](https://dplyr.tidyverse.org/reference/slice.html)             | [`slice_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)          | [`slice_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)          | Select rows by position                         |
| [`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html)        | [`slice_head_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)     | [`slice_head_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)     | Select first n rows                             |
| [`slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html)        | [`slice_tail_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)     | [`slice_tail_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)     | Select last n rows                              |
| [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)      | [`slice_sample_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)   | [`slice_sample_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)   | Select random rows                              |
| [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html)         | [`slice_max_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)      | [`slice_max_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)      | Select rows with highest values                 |
| [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html)         | [`slice_min_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)      | [`slice_min_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)      | Select rows with lowest values                  |
| [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)  | [`left_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | [`left_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | Add new columns from another table (left join)  |
| [`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html) | [`inner_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md) | [`inner_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md) | Add new columns from another table (inner join) |
| [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)  | [`semi_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | [`semi_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | Filter rows from another table (semi join)      |
| [`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)  | [`anti_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | [`anti_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)  | Filter rows from another table (anti join)      |

**The magic ingredient?** Every single one of these functions
automatically updates the expression matrix to match your metadata
operations. Filter out half your samples? The matrix follows suit.
Rearrange your variables? The matrix dances to the same tune.

**What about other `dplyr` functions?** For functions not directly
supported (like
[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html),
[`pull()`](https://dplyr.tidyverse.org/reference/pull.html),
[`count()`](https://dplyr.tidyverse.org/reference/count.html), etc.),
simply extract the tibble first and go wild:

``` r
# Extract the tibble, then use any dplyr function you want
toy_exp |>
  get_sample_info() |>
  distinct(group)

toy_exp |>
  get_var_info() |>
  pull(protein) |>
  unique()

toy_exp |>
  get_sample_info() |>
  count(group)
```

## The Sacred Index Columns - Handle with Care

Remember those index columns we mentioned? Here’s the golden rule:
**Don’t mess with them directly!** The reason is simple: `glyexp` relies
on them to synchronize expression matrix, sample info, and variable
info. Look at this picture to understand it better:

![](experiment.png)

Think of them as the foundation of your data house - you can redecorate
all you want, but don’t touch the support beams.

Want to select specific columns from your sample info? Easy:

``` r
toy_exp |>
  select_obs(group) |>
  get_sample_info()
#> # A tibble: 6 × 2
#>   sample group
#>   <chr>  <chr>
#> 1 S1     A    
#> 2 S2     A    
#> 3 S3     A    
#> 4 S4     B    
#> 5 S5     B    
#> 6 S6     B
```

See how the “sample” column (our trusty index) stuck around? That’s
`glyexp` being protective of your data integrity.

Even when you try to be sneaky, it’s got your back:

``` r
toy_exp |>
  select_obs(-starts_with("sample")) |>
  get_sample_info()
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

Nice try, but that index column isn’t going anywhere! 😄

## Slicing and Dicing - Matrix-Style Subsetting

Want to subset your experiment? Think matrix indexing, but smarter:

``` r
subset_exp <- toy_exp[, 1:3]
```

This grabs the first 3 samples, and like a good butler, updates
everything else accordingly:

``` r
get_expr_mat(subset_exp)
#>    S1 S2 S3
#> V1  1  5  9
#> V2  2  6 10
#> V3  3  7 11
#> V4  4  8 12
```

``` r
get_sample_info(subset_exp)
#> # A tibble: 3 × 3
#>   sample group batch
#>   <chr>  <chr> <dbl>
#> 1 S1     A         1
#> 2 S2     A         2
#> 3 S3     A         1
```

Both the expression matrix and sample info are perfectly in sync. It’s
like they’re dancing to the same tune!

## Merging and Splitting - The Dynamic Duo

Imagine this: you run your favorite glycopeptide identification software
two times on two batches, and you use `glyread` to load the results into
two \[experiment()\]s. How do you combine them into a single
\[experiment()\]? The answer is
[`merge()`](https://rdrr.io/r/base/merge.html):

``` r
merge(exp1, exp2)
```

What it does is quite complex, but you can rely on
[`merge()`](https://rdrr.io/r/base/merge.html) to handle it for you. If
you want to keep the batch information, you can use
[`mutate_obs()`](https://glycoverse.github.io/glyexp/reference/mutate_obs.md)
to add an ID column before merging.

What if you have more than one experiment to merge? Put them in a list
and use
[`purrr::reduce()`](https://purrr.tidyverse.org/reference/reduce.html)
to merge them:

``` r
purrr::reduce(list(exp1, exp2, exp3), merge)
```

The opposite of [`merge()`](https://rdrr.io/r/base/merge.html) is
[`split()`](https://rdrr.io/r/base/split.html), which splits an
\[experiment()\] into a list of \[experiment()\]s. You can provide a
column to split by, and the unique values of that column will be used as
the names of the list.

``` r
split(toy_exp, group, where = "sample_info")
#> $A
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
#> 
#> $B
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 4 variables
#> ℹ Sample information fields: group <chr>, batch <dbl>
#> ℹ Variable information fields: protein <chr>, peptide <chr>, glycan_composition <chr>
```

Now the “A” experiment only contains samples from group “A”, and “B”
experiment from group “B”.

## When You Need to Break Free - The Tibble Escape Hatch

The `glycoverse` ecosystem is pretty comprehensive, but we know there
are times when you need to venture beyond our cozy world. When that
moment comes, you can always fall back to basic R data structures by
[`get_expr_mat()`](https://glycoverse.github.io/glyexp/reference/get_expr_mat.md),
[`get_sample_info()`](https://glycoverse.github.io/glyexp/reference/get_sample_info.md),
and
[`get_var_info()`](https://glycoverse.github.io/glyexp/reference/get_var_info.md).

Alternatively, use
[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
to convert your
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
to a tibble in
[tidy-format](https://r4ds.hadley.nz/data-tidy.html#sec-tidy-data):

``` r
as_tibble(toy_exp)
#> # A tibble: 24 × 8
#>    sample group batch variable protein peptide glycan_composition value
#>    <chr>  <chr> <dbl> <chr>    <chr>   <chr>   <chr>              <int>
#>  1 S1     A         1 V1       PRO1    PEP1    H5N2                   1
#>  2 S2     A         2 V1       PRO1    PEP1    H5N2                   5
#>  3 S3     A         1 V1       PRO1    PEP1    H5N2                   9
#>  4 S4     B         2 V1       PRO1    PEP1    H5N2                  13
#>  5 S5     B         1 V1       PRO1    PEP1    H5N2                  17
#>  6 S6     B         2 V1       PRO1    PEP1    H5N2                  21
#>  7 S1     A         1 V2       PRO2    PEP2    H5N2                   2
#>  8 S2     A         2 V2       PRO2    PEP2    H5N2                   6
#>  9 S3     A         1 V2       PRO2    PEP2    H5N2                  10
#> 10 S4     B         2 V2       PRO2    PEP2    H5N2                  14
#> # ℹ 14 more rows
```

**Pro tip:** These tibbles can get *really* long (think novel-length),
especially with all that rich metadata. Smart analysts filter their
experiments first:

``` r
toy_exp |>
  filter_var(glycan_composition == "H5N2") |>
  select_obs(group) |>
  select_var(-glycan_composition) |>
  as_tibble()
#> # A tibble: 12 × 6
#>    sample group variable protein peptide value
#>    <chr>  <chr> <chr>    <chr>   <chr>   <int>
#>  1 S1     A     V1       PRO1    PEP1        1
#>  2 S2     A     V1       PRO1    PEP1        5
#>  3 S3     A     V1       PRO1    PEP1        9
#>  4 S4     B     V1       PRO1    PEP1       13
#>  5 S5     B     V1       PRO1    PEP1       17
#>  6 S6     B     V1       PRO1    PEP1       21
#>  7 S1     A     V2       PRO2    PEP2        2
#>  8 S2     A     V2       PRO2    PEP2        6
#>  9 S3     A     V2       PRO2    PEP2       10
#> 10 S4     B     V2       PRO2    PEP2       14
#> 11 S5     B     V2       PRO2    PEP2       18
#> 12 S6     B     V2       PRO2    PEP2       22
```

Much more manageable, right?

## Standing on the Shoulders of Giants

Designing
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
wasn’t done in a vacuum - we learned from some amazing predecessors:

**SummarizedExperiment** 📊  
The granddaddy of omics data containers from
[Bioconductor](https://github.com/Bioconductor/SummarizedExperiment).
Solid as a rock for RNA-seq, but not quite “tidy” enough for our taste.

**tidySummarizedExperiment** 🧹  
A brilliant attempt to bring tidy principles to SummarizedExperiment
from the
[tidySummarizedExperiment](https://github.com/tidyomics/tidySummarizedExperiment)
package. We love the concept, but felt that cramming everything into one
tibble doesn’t quite capture the mental model of separated data types.

**massdataset** 🔬  
Our closest cousin! The
[massdataset](https://github.com/tidymass/massdataset) package gets so
many things right - tidy operations, clean data separation, perfect for
mass spec data. We especially admire its data processing history
tracking (reproducibility FTW!).

But here’s our twist: while object-oriented programming has its merits,
we believe most R users think functionally. Your code *is* your
reproducibility trail - elegant, transparent, and familiar to every R
user.

**Our Philosophy** 💭  
We chose the functional programming path because it feels like home to R
users. No hidden states, no mysterious transformations - just clear,
chainable functions that do exactly what they say on the tin.

------------------------------------------------------------------------

*Huge thanks to all the developers who paved this road. `glyexp` exists
because of your groundbreaking work! 🙏*

## What’s Next?

Now you have the basic understanding of
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).
Next, you can learn how to use
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
in your analysis.

- [Creating
  Experiments](https://glycoverse.github.io/glyexp/articles/create-exp.html)
- [Experiment
  Types](https://glycoverse.github.io/glyexp/articles/exp-type.html)
- [Dplyr-Style
  Functions](https://glycoverse.github.io/glyexp/articles/dplyr-style-functions.html)
