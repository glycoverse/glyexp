# Create a GlycomicSE object

**\[experimental\]**

`GlycomicSE()` creates a single-assay
[`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
subclass for glycomics data. It is a thin wrapper around
`SummarizedExperiment()` with additional Glycoverse validation:

1.  Exactly one assay is allowed. Extra assays are rejected to avoid
    ambiguous glycomics measurements.

2.  The assay must contain only non-negative values. Raw glycomics
    abundance data are non-negative; log transformation should be
    handled by downstream Glycoverse packages.

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
    of `"N"`, `"O-GalNAc"`, `"O-GlcNAc"`, `"O-Man"`, `"O-Fuc"`, or
    `"O-Glc"`.

## Value

A `GlycomicSE` object.

## S4 class

`GlycomicSE` is an S4 class that extends
[`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).

## See also

[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
