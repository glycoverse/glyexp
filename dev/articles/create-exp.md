# Creating Experiments

This vignette describes how to create an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object from scratch.

> **Note:** If you’re using `glyread` to import your data, sample
> information is already handled, and you can focus on understanding the
> structure.

``` r
library(glyexp)
library(glyrepr)
library(tibble)
```

## Required Components

Creating an
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
object requires three key components:

- **Expression Matrix**: Numeric data with variables as rows and samples
  as columns
- **Sample Information**: A tibble containing sample metadata (groups,
  batches, etc.)
- **Variable Information**: A tibble describing variables (proteins,
  peptides, glycan compositions, etc.)

You also need to specify experiment metadata.

## Step 1: Sample Information

The sample information table records metadata for each sample.

The golden rule here is simple: your first column **must** be named
`sample`, and every entry needs to be unique (no duplicates allowed!).
These are your sample identifiers – the names that tie everything
together.

For the remaining columns, you have complete freedom! Add any
information that matters for your analysis: groups, batches, patient
demographics, treatment conditions – whatever helps tell your story.

### Recommended Column Names

The glycoverse ecosystem uses these standard column names:

- **`group`**: Experimental conditions or treatments (use factor type).
  Most `glystats` functions rely on this column.
- **`batch`**: Sample batch information (use factor type). Essential for
  `glyclean::remove_batch_effect()`. If no batch column exists,
  `glyclean::auto_clean()` skips batch correction.
- **`bio_rep`**: Biological replicates (use factor type).

> **Note:** The function automatically coerces column types to expected
> types if they don’t match. This behavior can be controlled by the
> `coerce_col_types` and `check_col_types` arguments. Manual conversion
> is recommended for finer control over details like factor levels.

Let’s build our sample information table:

``` r
sample_info <- tibble(
  sample = c("S1", "S2", "S3", "S4", "S5", "S6"),
  group = factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
  batch = factor(c(1, 2, 1, 2, 1, 2), levels = c(1, 2))
)
sample_info
#> # A tibble: 6 × 3
#>   sample group batch
#>   <chr>  <fct> <fct>
#> 1 S1     A     1    
#> 2 S2     A     2    
#> 3 S3     A     1    
#> 4 S4     B     2    
#> 5 S5     B     1    
#> 6 S6     B     2
```

## Step 2: Variable Information

The variable information table describes what each measurement
represents.

**For glycoproteomics:** If you’re using `glyread`, variable information
is automatically extracted from software output files.

**For glycomics:** Variable information may need to be built manually.
Glycomics data structures are simpler than glycoproteomics.

### Variable Naming

The first column must be named `variable` with unique values. Names
don’t need to be meaningful - `glyread` uses simple names like “V1”,
“V2”, “V3” by default.

### Column Conventions by Experiment Type

**For glycomics experiments:**

- **`glycan_composition`**: Required. Glycan composition as a
  [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html)
  object
- **`glycan_structure`**: Optional. Glycan structure as a
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  object (Parsed objects are recommended over character strings to avoid
  repeated parsing)

**For glycoproteomics experiments:**

- **`protein`**: Required. UniProt accession (character)
- **`protein_site`**: Required. Glycosylation site position on the
  protein (integer)
- **`gene`**: Optional. Gene name (character)
- **`peptide`**: Optional. Peptide sequence (character)
- **`peptide_site`**: Optional. Peptide site position for glycan
  attachment (integer)

Let’s create our variable information table:

``` r
var_info <- tibble(
  variable = c("V1", "V2", "V3"),
  glycan_composition = glyrepr::glycan_composition(
    c(GalNAc = 1),
    c(Gal = 1, GalNAc = 1),
    c(Gal = 1, GalNAc = 1, GlcNAc = 1)
  ),
  glycan_structure = glyrepr::as_glycan_structure(c(
    "GalNAc(a1-",
    "Gal(b1-3)GalNAc(a1-",
    "Gal(b1-3)[GlcNAc(a1-6)]GalNAc(a1-"
  ))
)
var_info
#> # A tibble: 3 × 3
#>   variable glycan_composition       glycan_structure                 
#>   <chr>    <comp>                   <struct>                         
#> 1 V1       GalNAc(1)                GalNAc(a1-                       
#> 2 V2       Gal(1)GalNAc(1)          Gal(b1-3)GalNAc(a1-              
#> 3 V3       Gal(1)GlcNAc(1)GalNAc(1) Gal(b1-3)[GlcNAc(a1-6)]GalNAc(a1-
```

## Step 3: Expression Matrix

The expression matrix contains your actual measurements. Layout:
variables as rows, samples as columns.

No transformation (like log-transformation) is needed - glycoverse
functions handle this internally if required.

### Matching Requirements

Row names must match the `variable` column from variable information,
and column names must match the `sample` column from sample information.
The order doesn’t need to be perfect - functions handle alignment
automatically.

Let’s create our expression matrix:

``` r
# Create a simple matrix with 3 variables and 6 samples
expr_mat <- matrix(
  rnorm(18, mean = 10, sd = 2), # Some realistic-looking data
  nrow = 3, 
  ncol = 6
)
rownames(expr_mat) <- var_info$variable
colnames(expr_mat) <- sample_info$sample
expr_mat
#>           S1        S2       S3        S4        S5       S6
#> V1  7.199913  9.988857 6.356365  9.434589 14.130050 6.273977
#> V2 10.510634 11.243105 9.505349  8.892601  6.738021 8.955975
#> V3  5.125473 12.296823 9.511601 11.257964 11.024854 9.894796
```

## Step 4: Creating the Experiment

Now assemble the experiment object:

``` r
exp <- experiment(expr_mat, sample_info, var_info, exp_type = "glycomics", glycan_type = "N")
exp
#> 
#> ── Glycomics Experiment ────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 3 variables
#> ℹ Sample information fields: group <fct>, batch <fct>
#> ℹ Variable information fields: glycan_composition <comp>, glycan_structure <struct>
```

Specify the experiment type (`"glycomics"` or `"glycoproteomics"`) and
glycan type (like `"N"` for N-glycans, `"O-GalNAc"` for O-GalNAc
glycans, etc.). These details help downstream functions interpret the
data correctly.

## Minimum Required Input

The minimum required input is only an expression matrix.

``` r
expr_mat <- matrix(runif(9), nrow = 3, ncol = 3)
colnames(expr_mat) <- c("S1", "S2", "S3")
rownames(expr_mat) <- c("V1", "V2", "V3")
experiment(expr_mat)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 3 variables
#> ℹ Sample information fields: none
#> ℹ Variable information fields: none
```

If any other information is not provided, it will be automatically
generated based on the following rules:

- `sample_info`: a tibble with only one column named “sample”, same as
  the column names of `expr_mat`.
- `var_info`: a tibble with only one column named “variable”, same as
  the row names of `expr_mat`.
- `exp_type`: “others”
- `glycan_type`: `NULL`

This means you can create
[`experiment()`](https://glycoverse.github.io/glyexp/dev/reference/experiment.md)
objects flexibly: first create the backbone using an expression matrix,
then add information later using
[`mutate_var()`](https://glycoverse.github.io/glyexp/dev/reference/mutate_obs.md)
and
[`mutate_obs()`](https://glycoverse.github.io/glyexp/dev/reference/mutate_obs.md).
If you have data in multiple tables, use
[`left_join_var()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)
and
[`left_join_obs()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)
to join them.
