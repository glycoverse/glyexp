# Creating Experiments

This vignette will guide you through creating an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object from scratch. Think of it as your step-by-step recipe for
building the foundation of your glycomics analysis!

> **Quick tip:** If you’re already using `glyread` to import your data,
> you can focus on the “Sample Information” section and breeze through
> the rest.

``` r
library(glyexp)
library(glyrepr)
library(tibble)
```

## What You’ll Need

Creating an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object is like assembling a puzzle – you need three key pieces to make
everything fit together perfectly:

- **Expression Matrix**: Your numeric data arranged with variables as
  rows and samples as columns (the heart of your dataset!)
- **Sample Information**: A tibble containing all the juicy details
  about your samples – groups, batches, demographics, you name it
- **Variable Information**: A tibble describing your variables –
  proteins, peptides, glycan compositions, and more

You’ll also sprinkle in some experiment metadata to complete the
picture.

Ready? Let’s dive in!

## Step 1: Getting Your Samples Organized

Let’s start with the sample information – think of this as your sample’s
ID card plus all the important details about each one.

The golden rule here is simple: your first column **must** be named
`sample`, and every entry needs to be unique (no duplicates allowed!).
These are your sample identifiers – the names that tie everything
together.

For the remaining columns, you have complete freedom! Add any
information that matters for your analysis: groups, batches, patient
demographics, treatment conditions – whatever helps tell your story.

### Pro Tips for Column Names

The `glycoverse` family has some favorite column names that make
everything work seamlessly:

- **`group`**: Your experimental conditions or treatments (and yes, make
  it a factor!).  
  This is your star player – most `glystats` functions rely on this
  column.
- **`batch`**: For tracking sample batches (another factor, please!).  
  Essential for `glyclean::remove_batch_effect()`. No batch column? No
  problem – `glyclean::auto_clean()` will simply skip batch correction.
- **`bio_rep`**: For biological replicates (a factor!). May be used in
  the future.

> **Note:** We’re constantly expanding this list, so stay tuned for more
> conventions! **Another note:** If you “accidentally” did not perfectly
> follow the column typing conventions, the function will automatically
> coerce the column types to the expected types. This behavior can be
> controlled by the `coerce_col_types` and `check_col_types` arguments.
> Although you can rely on automatic column type conversion, we still
> recommend you manually convert the column types to the expected types.
> This allows you a finer control over the details, for example, the
> factor levels.

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

## Step 2: Describing Your Variables

Now for the variable information – this is where you describe what each
measurement actually represents.

**Good news for glycoproteomics folks:** If you’re using `glyread`, sit
back and relax! It automatically extracts all this information from your
software output files.

**For glycomics experiments:** You’ll likely need to build this
manually, but don’t worry – glycomics data structures are much more
straightforward than their glycoproteomics cousins.

### The Variable Naming Game

Just like with samples, your first column **must** be named `variable`
with unique values. Here’s the thing though – these names don’t need to
be fancy or meaningful. In fact, `glyread` happily uses simple names
like “V1”, “V2”, “V3” by default, and that works perfectly fine!

### Column Conventions by Experiment Type

**For glycomics experiments**, these columns are your friends:

- **`glycan_composition`**: Required. Your glycan composition as a
  [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html)
  object
- **`glycan_structure`**: Optional. Your glycan structure as a
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  object  
  (You *could* use character strings that `glyparse::auto_parse()`
  understands, but we recommend the parsed objects to avoid repetitive
  parsing later)

**For glycoproteomics experiments**, you’ll want these additional
columns:

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

## Step 3: Building Your Expression Matrix

Time for the main event – your expression matrix! This is where all your
actual measurements live.

The layout is straightforward: **variables as rows, samples as
columns**.

You don’t need to perform any transformation on your data, especially
log-transformation. `glycoverse` functions will handle the data
transformation internally if needed.

### The Matching Game

Here’s the important bit: your row names must match the `variable`
column from your variable information, and your column names must match
the `sample` column from your sample information. Think of it as
connecting the dots between your data and metadata.

**Bonus:** The order doesn’t need to be perfect – the functions are
smart enough to line everything up correctly!

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

## Step 4: Bringing It All Together

Congratulations! You’ve got all the pieces of the puzzle. Now let’s
assemble your
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object:

``` r
exp <- experiment(expr_mat, sample_info, var_info, exp_type = "glycomics", glycan_type = "N")
exp
#> 
#> ── Glycomics Experiment ────────────────────────────────────────────────────────
#> ℹ Expression matrix: 6 samples, 3 variables
#> ℹ Sample information fields: group <fct>, batch <fct>
#> ℹ Variable information fields: glycan_composition <comp>, glycan_structure <struct>
```

**Don’t forget the metadata:** You’ll need to specify your experiment
type (`"glycomics"` or `"glycoproteomics"`) and glycan type (like `"N"`
for N-glycans, `"O-GalNAc"` for O-GalNAc glycans, etc.). These little
details help downstream functions understand exactly what they’re
working with.

And there you have it – your very own
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object, ready for all the exciting analyses that await! 🎉

## The Minimum Required Input

In fact, the minimum required input is only an expression matrix.

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
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
objects in a more flexible way: first create the backbone using an
expression matrix, then add more information later using
[`mutate_var()`](https://glycoverse.github.io/glyexp/reference/mutate_obs.md)
and
[`mutate_obs()`](https://glycoverse.github.io/glyexp/reference/mutate_obs.md).
If you have multiple fields stored in different tables, you can use
[`left_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)
and
[`left_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)
to join them to the
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).
