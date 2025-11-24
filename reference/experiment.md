# Create a new experiment

The data container of a glycoproteomics or glycomics experiment.
Expression matrix, sample information, and variable information are
required then will be managed by the experiment object. It acts as the
data core of the `glycoverse` ecosystem.

## Usage

``` r
experiment(
  expr_mat,
  sample_info = NULL,
  var_info = NULL,
  exp_type = "others",
  glycan_type = NULL,
  coerce_col_types = TRUE,
  check_col_types = TRUE,
  ...
)

is_experiment(x)
```

## Arguments

- expr_mat:

  An expression matrix with samples as columns and variables as rows.

- sample_info:

  A tibble with a column named "sample", and other columns other useful
  information about samples, e.g. group, batch, sex, age, etc. If NULL
  (default), a tibble with only one column named "sample" will be
  created, same as the column names of `expr_mat`.

- var_info:

  A tibble with a column named "variable", and other columns other
  useful information about variables, e.g. protein name, peptide, glycan
  composition, etc. If NULL (default), a tibble with only one column
  named "variable" will be created, same as the row names of `expr_mat`.
  Must be provided if `exp_type` is not "others".

- exp_type:

  The type of the experiment, "glycomics", "glycoproteomics",
  "traitomics", "traitproteomics", or "others". Default to "others".

- glycan_type:

  The type of glycan. One of "N", "O-GalNAc", "O-GlcNAc", "O-Man",
  "O-Fuc", "O-Glc". Can also be NULL if `exp_type` is "others".

- coerce_col_types:

  If common column types are coerced. Default to TRUE. If TRUE, all
  columns in the "Column conventions" section will be coerced to the
  expected types. Skipped for "others" type even if TRUE.

- check_col_types:

  If column type conventions are checked. Default to TRUE. Type checking
  is performed after column coercion (if `coerce_col_types` is TRUE).
  Skipped for "others" type even if TRUE.

- ...:

  Other meta data about the experiment.

- x:

  An object to check.

## Value

A `experiment()`. If the input data is wrong, an error will be raised.

A logical value.

## Requirements of the input data

**Expression matrix:**

- Must be a numeric matrix with **variables as rows** and **samples as
  columns**.

- The **column names** must correspond to sample IDs.

- The **row names** must correspond to variable IDs.

**Sample information (`sample_info`):**

- Must be a tibble with a column named "sample" (sample ID).

- Each value in "sample" must be unique.

- The set of "sample" values must match the column names of the
  expression matrix (order does not matter).

**Variable information (`var_info`):**

- Must be a tibble with a column named "variable" (variable ID).

- Each value in "variable" must be unique.

- The set of "variable" values must match the row names of the
  expression matrix (order does not matter).

The function will automatically reorder the expression matrix to match
the order of "sample" and "variable" in the info tables.

## Column requirements

Some columns are required compulsorily in the variable information
tibble for a valid experiment. It depends on the experiment type.

- For "glycomics": `glycan_composition`.

- For "glycoproteomics": `protein`, `protein_site`,
  `glycan_composition`.

- For "traitomics": no required columns.

- For "traitproteomics": `protein`, `protein_site`.

- For "others": no required columns.

See the "Column conventions" section for detailed description of these
columns.

The last two types of experiments are created by the `glydet` package.
Normally you don't need to manually create them.

## Column conventions

`glycoverse` has some conserved column names for `sample_info` and
`var_info` to make everything work seamlessly. It's not mandatory, but
following these conventions will make your life easier.

**sample_info:**

- `group`: factor, treatment/condition/grouping, used by many `glystats`
  and `glyvis` functions.

- `batch`: factor, batch information, used by
  `glyclean::correct_batch_effect()`.

- `bio_rep`: factor, biological replicate, may be used in the future.

**var_info:**

- `protein`: character, protein Uniprot accession.

- `protein_site`: integer, glycosylation site position on protein.

- `gene`: character, gene symbol.

- `peptide`: character, peptide sequence.

- `peptide_site`: integer, glycosylation site position on peptide.

- `glycan_composition`:
  [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html),
  glycan composition.

- `glycan_structure`:
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html),
  glycan structure.

## Meta data

Other meta data can be added to the `meta_data` attribute. `meta_data`
is a list of additional information about the experiment. Two meta data
fields are required:

- `exp_type`: "glycomics", "glycoproteomics", "traitomics",
  "traitproteomics", or "others"

- `glycan_type`: "N", "O-GalNAc", "O-GlcNAc", "O-Man", "O-Fuc", "O-Glc",
  or NULL

Other meta data will be added by other `glycoverse` packages for their
own purposes.

## Index columns

The **index columns** are the backbone that keep your data synchronized:

- The "sample" column in `sample_info` must match the column names of
  `expr_mat`.

- The "variable" column in `var_info` must match the row names of
  `expr_mat`.

These columns act as unique identifiers, ensuring that your expression
matrix, sample information, and variable information always stay in
sync, no matter how you filter, arrange, or subset your data.

## Examples

``` r
# The minimum required input is an expression matrix.
expr_mat <- matrix(runif(9), nrow = 3, ncol = 3)
colnames(expr_mat) <- c("S1", "S2", "S3")
rownames(expr_mat) <- c("V1", "V2", "V3")
experiment(expr_mat)
#> 
#> ── Others Experiment ───────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 3 variables
#> ℹ Sample information fields: none
#> ℹ Variable information fields: none

# Or with more detailed information.
sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"), group = c("A", "B", "A"))
var_info <- tibble::tibble(
  variable = c("V1", "V2", "V3"),
  glycan_composition = glyrepr::glycan_composition(c(Hex = 1))
)
experiment(expr_mat, sample_info, var_info, exp_type = "glycomics", glycan_type = "N")
#> Column group converted to <factor>.
#> 
#> ── Glycomics Experiment ────────────────────────────────────────────────────────
#> ℹ Expression matrix: 3 samples, 3 variables
#> ℹ Sample information fields: group <fct>
#> ℹ Variable information fields: glycan_composition <comp>
```
