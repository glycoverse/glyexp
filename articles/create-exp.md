# Creating Experiments

This vignette describes how to create an experiment object from scratch.

> **Note:** If you’re using `glyread` to import your data, sample
> information is already handled, and you can focus on understanding the
> structure.

## Required Components

Creating an experiment object requires three key components:

- **Expression Matrix**: Numeric data with variables as rows and samples
  as columns
- **Sample Information**: A `DataFrame` containing sample metadata
  (groups, batches, etc.)
- **Variable Information**: A `DataFrame` describing variables
  (proteins, peptides, glycan compositions, etc.)

You also need to specify experiment metadata.

Now let’s make a glycomic experiment together.

## Step 1: Sample Information (Column Data)

The sample information table records metadata for each sample.
Conditions, groupings, batches… Anything describe your samples can be
put here.

We recommend you at least include a `group` column as a factor for your
conditions, which will be used throughout `glyclean`, `glystats`, and
`glyvis`.

Let’s build our sample information table:

``` r

col_data <- DataFrame(
  group = factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
  batch = factor(c(1, 2, 1, 2, 1, 2), levels = c(1, 2)),
  row.names = c("S1", "S2", "S3", "S4", "S5", "S6")
)
col_data
#> DataFrame with 6 rows and 2 columns
#>       group    batch
#>    <factor> <factor>
#> S1        A        1
#> S2        A        2
#> S3        A        1
#> S4        B        2
#> S5        B        1
#> S6        B        2
```

## Step 2: Variable Information (Row Data)

The variable information table describes what each measurement
represents. If you use the `glyread` package, the variable information
table will be automatically generated. You can also provide your own
variable information table.

For `GlycomicSE`, at least the following columns should be present:

- **`glycan_composition`**: Required. Glycan composition as a
  [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html)
  object

For `GlycoproteomicSE`, at least the following columns should be
present:

- **`protein`**: Required. UniProt accession (character)
- **`protein_site`**: Required. Glycosylation site position on the
  protein (integer)
- **`glycan_composition`**: Required. Glycan composition as a
  [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html)
  object

If glycan structures are available, an additional `glycan_structure`
column as a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object can also be provided in the data frame.

Let’s create our variable information table:

``` r

row_data <- DataFrame(
  glycan_composition = glyrepr::glycan_composition(
    c(GalNAc = 1),
    c(Gal = 1, GalNAc = 1),
    c(Gal = 1, GalNAc = 1, GlcNAc = 1)
  ),
  glycan_structure = glyrepr::as_glycan_structure(c(
    "GalNAc(a1-",
    "Gal(b1-3)GalNAc(a1-",
    "Gal(b1-3)[GlcNAc(a1-6)]GalNAc(a1-"
  )),
  row.names = c("V1", "V2", "V3")
)
row_data
#> DataFrame with 3 rows and 2 columns
#>        glycan_composition       glycan_structure
#>     <glyrepr_composition>    <glyrepr_structure>
#> V1              GalNAc(1)             GalNAc(a1-
#> V2        Gal(1)GalNAc(1)    Gal(b1-3)GalNAc(a1-
#> V3 Gal(1)GlcNAc(1)GalNA.. Gal(b1-3)[GlcNAc(a1-..
```

## Step 3: Expression Matrix

The expression matrix contains your actual measurements. Layout:
variables as rows, samples as columns.

No transformation (like log-transformation) is needed - glycoverse
functions handle this internally if required.

### Matching Requirements

Row names must match row names of the variable information table, and
column names must match the row names of the sample information table,
in the same order.

Let’s create our expression matrix:

``` r

# Create a simple matrix with 3 variables and 6 samples
expr_mat <- matrix(
  rnorm(18, mean = 10, sd = 2), # Some realistic-looking data
  nrow = 3, 
  ncol = 6
)
rownames(expr_mat) <- rownames(row_data)
colnames(expr_mat) <- rownames(col_data)
expr_mat
#>           S1        S2       S3        S4        S5       S6
#> V1  7.199913  9.988857 6.356365  9.434589 14.130050 6.273977
#> V2 10.510634 11.243105 9.505349  8.892601  6.738021 8.955975
#> V3  5.125473 12.296823 9.511601 11.257964 11.024854 9.894796
```

## Step 4: Creating the Experiment

Now assemble the experiment object:

``` r

exp <- GlycomicSE(expr_mat, colData = col_data, rowData = row_data, metadata = list(glycan_type = "N"))
exp
#> 
#> ── GlycomicSE ──────────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 6 samples, 3 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: glycan_composition <comp>, glycan_structure <struct>
#> ℹ Column data fields: group <fct>, batch <fct>
#> ℹ Metadata fields: glycan_type <chr>
```

Note that you should also provided a list as the `metadata`. The
`glycan_type` field is required, and you can provide others to serve
your old need.
