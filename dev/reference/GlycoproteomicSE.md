# Create a GlycoproteomicSE object

`GlycoproteomicSE()` creates a single-assay
[`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
subclass for glycoproteomics data. It is a thin wrapper around
`SummarizedExperiment()` with additional Glycoverse validation:

1.  Exactly one assay is allowed. Extra assays are rejected to avoid
    ambiguous glycoproteomics measurements.

2.  The assay must contain only non-negative values when `scale` is
    `"raw"`. Negative values are allowed for `"log"`, `"log2"`,
    `"log10"`, and `"logx"` scales.

3.  `rowData` must contain `protein`, `protein_site`, and
    `glycan_composition` columns. The `protein` column must be
    character, `protein_site` must be integer-like, and
    `glycan_composition` must be a
    [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html)
    vector. A `glycan_structure` column is optional, but if present it
    must be a
    [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
    vector.

4.  `metadata` must contain a `glycan_type` field.

This container is experimental and is not recognized by all Glycoverse
packages yet. It is intended to become a recommended entry point as
package contracts migrate toward `SummarizedExperiment`-based
containers.

## Usage

``` r
GlycoproteomicSE(abundance, ...)

# S4 method for class 'GlycoproteomicSE,list'
dimnames(x) <- value

# S4 method for class 'GlycoproteomicSE,NULL'
dimnames(x) <- value

# S4 method for class 'GlycoproteomicSE'
rownames(x) <- value

# S4 method for class 'GlycoproteomicSE'
colnames(x) <- value
```

## Arguments

- abundance:

  A numeric abundance matrix with glycopeptides or glycoforms as rows
  and samples as columns.

- ...:

  Arguments passed to
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).

  - `rowData`: A
    [`S4Vectors::DataFrame()`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html)
    with at least the following columns:

    - `protein`: required, a character vector

    - `protein_site`: required, an integer-like vector

    - `glycan_composition`: required, a
      [`glyrepr::glycan_composition()`](https://glycoverse.github.io/glyrepr/reference/glycan_composition.html)
      vector

    - `glycan_structure`: optional, a
      [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
      vector

  - `colData`: A
    [`S4Vectors::DataFrame()`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html).

  - `metadata`: A list. It must include a `glycan_type` field with one
    of `"N"`, `"O"`, `"O-GalNAc"`, `"O-Man"`, `"O-Fuc"`, `"O-GlcNAc"`,
    `"O-Glc"`, `"HMO"`, `"GSL"`, `"GAG"`, `"GPI"`, or `"others"`. The
    optional `scale` field is `"raw"` (original linear scale, the
    default) or `"log"` (natural logarithm), `"log2"`, `"log10"`, or
    `"logx"`. `"logx"` denotes a transformed logarithmic or log-like
    analysis scale (for example, CLR, ALR, or VSN) whose original
    abundance cannot be inferred or restored automatically from `scale`
    alone. It does not imply a common mathematical interpretation or
    log2 fold changes. This field describes the supplied data; setting
    it does not transform the assay or migrate existing objects.

- x:

  A `GlycoproteomicSE` object whose names are replaced.

- value:

  Replacement names, or a two-element list for `dimnames<-`.

## Value

A `GlycoproteomicSE` object.

## Details

Older development objects used `"log"` without specifying a base. Update
their metadata to `"log2"` or `"log10"` if that was the transformation
applied. Constructors never infer the base or transform abundance
values.

## S4 class

`GlycoproteomicSE` is an S4 class that extends
[`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).

## Row and column names

Rows (variables) and columns (samples) are handled independently.
Supplied names must be complete (no `NA` or empty strings) and unique
within each axis. When abundance and the corresponding annotation table
both have names, their name sets must match exactly; `rowData` or
`colData` is reordered to match abundance, without reordering the
abundance values. When only one component has names, the other inherits
them by position. When neither has names, variable names are generated
as `V1`, `V2`, ... and sample names as `S1`, `S2`, ... . Annotation
tables must have the corresponding number of rows.

We recommend supplying matching names on all three components whenever
possible: row and column names on abundance, variable row names on
`rowData`, and sample row names on `colData`. This lets the constructor
detect missing or mismatched identities early; a different order is
aligned automatically. Omitted names remain supported through the
inheritance and generation rules. Ordinary `variable` and `sample`
annotation columns are not identity keys.

The resolved names are stored on the object, annotation tables, and
underlying assay. Even `assay(x, withDimnames = FALSE)` has the resolved
names. The `checkDimnames` argument cannot disable these naming rules.
Replacing names with rownames\<-, colnames\<-, or dimnames\<- requires
complete, unique names of the correct length and synchronizes the
underlying assay without moving abundance values or annotations. Use the
conversion helpers to normalize names on an older object; duplicates or
incomplete supplied names must be corrected explicitly before
conversion.

## See also

[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
