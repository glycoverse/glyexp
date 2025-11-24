# Join data to sample or variable information

These functions allow you to join additional data to the sample
information or variable information of an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).
They work similarly to
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
and
[`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
but are designed to work with experiment objects.

After joining, the `expr_mat` is automatically updated to reflect any
changes in the number of samples or variables.

**Important Notes:**

- The `relationship` parameter is locked to "many-to-one" to ensure that
  the number of observations never increases, which would violate the
  experiment object assumptions.

- `right_join()` and `full_join()` are not supported as they could add
  new observations to the experiment.

## Usage

``` r
left_join_obs(exp, y, by = NULL, ...)

inner_join_obs(exp, y, by = NULL, ...)

semi_join_obs(exp, y, by = NULL, ...)

anti_join_obs(exp, y, by = NULL, ...)

left_join_var(exp, y, by = NULL, ...)

inner_join_var(exp, y, by = NULL, ...)

semi_join_var(exp, y, by = NULL, ...)

anti_join_var(exp, y, by = NULL, ...)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

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

A new
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object with updated sample or variable information.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following object is masked from ‘package:glyexp’:
#> 
#>     select_var
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(tibble)

# Create a toy experiment
exp <- toy_experiment

# Create additional sample information to join
extra_sample_info <- tibble(
  sample = c("S1", "S2", "S3", "S4"),
  age = c(25, 30, 35, 40),
  treatment = c("A", "B", "A", "B")
)

# Left join to sample information
exp_with_extra <- left_join_obs(exp, extra_sample_info, by = "sample")
get_sample_info(exp_with_extra)
#> # A tibble: 6 × 5
#>   sample group batch   age treatment
#>   <chr>  <chr> <dbl> <dbl> <chr>    
#> 1 S1     A         1    25 A        
#> 2 S2     A         2    30 B        
#> 3 S3     A         1    35 A        
#> 4 S4     B         2    40 B        
#> 5 S5     B         1    NA NA       
#> 6 S6     B         2    NA NA       

# Inner join (only keeps matching samples)
exp_inner <- inner_join_obs(exp, extra_sample_info, by = "sample")
get_sample_info(exp_inner)
#> # A tibble: 4 × 5
#>   sample group batch   age treatment
#>   <chr>  <chr> <dbl> <dbl> <chr>    
#> 1 S1     A         1    25 A        
#> 2 S2     A         2    30 B        
#> 3 S3     A         1    35 A        
#> 4 S4     B         2    40 B        
get_expr_mat(exp_inner) # Note: expr_mat is updated too
#>    S1 S2 S3 S4
#> V1  1  5  9 13
#> V2  2  6 10 14
#> V3  3  7 11 15
#> V4  4  8 12 16

# Create additional variable information to join
extra_var_info <- tibble(
  protein = c("P1", "P2", "P3"),
  pathway = c("A", "B", "A"),
  importance = c(0.8, 0.6, 0.9)
)

# Left join to variable information
exp_with_var_extra <- left_join_var(exp, extra_var_info, by = "protein")
get_var_info(exp_with_var_extra)
#> # A tibble: 4 × 6
#>   variable protein peptide glycan_composition pathway importance
#>   <chr>    <chr>   <chr>   <chr>              <chr>        <dbl>
#> 1 V1       PRO1    PEP1    H5N2               NA              NA
#> 2 V2       PRO2    PEP2    H5N2               NA              NA
#> 3 V3       PRO3    PEP3    H3N2               NA              NA
#> 4 V4       PRO3    PEP4    H3N2               NA              NA
```
