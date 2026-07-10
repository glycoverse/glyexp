# Create a GlycoproteomicSE object

**\[experimental\]**

`GlycoproteomicSE()` creates a single-assay
[`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
subclass for glycoproteomics data. It is a thin wrapper around
`SummarizedExperiment()` with additional Glycoverse validation:

1.  Exactly one assay is allowed. Extra assays are rejected to avoid
    ambiguous glycoproteomics measurements.

2.  The assay must contain only non-negative values. Raw glycoproteomics
    abundance data are non-negative; log transformation should be
    handled by downstream Glycoverse packages.

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
    `"O-Glc"`, `"HMO"`, `"GSL"`, `"GAG"`, or `"GPI"`.

## Value

A `GlycoproteomicSE` object.

## S4 class

`GlycoproteomicSE` is an S4 class that extends
[`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).

## See also

[`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
