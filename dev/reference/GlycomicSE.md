# Create a GlycomicSE object

`GlycomicSE()` creates a single-assay
[`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
subclass for glycomics data. It is a thin wrapper around
`SummarizedExperiment()` with additional Glycoverse validation:

1.  Exactly one assay is allowed. Extra assays are rejected to avoid
    ambiguous glycomics measurements.

2.  The assay must contain only non-negative values when `scale` is
    `"raw"`. Negative values are allowed for `"log"`, `"log2"`, and
    `"log10"` scales.

3.  `rowData` must contain a `glycan_composition` column, and that
    column must be a
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
GlycomicSE(abundance, ...)
```

## Arguments

- abundance:

  A numeric abundance matrix with glycans as rows and samples as
  columns.

- ...:

  Arguments passed to
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).

  - `rowData`: A
    [`S4Vectors::DataFrame()`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html)
    with at least the following columns:

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
    default) or `"log"` (natural logarithm), `"log2"`, or `"log10"`.
    This field describes the supplied data; setting it does not
    transform the assay.

## Value

A `GlycomicSE` object.

## Details

Older development objects used `"log"` without specifying a base. Update
their metadata to `"log2"` or `"log10"` if that was the transformation
applied. Constructors never infer the base or transform abundance
values.

## S4 class

`GlycomicSE` is an S4 class that extends
[`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).

## See also

[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
