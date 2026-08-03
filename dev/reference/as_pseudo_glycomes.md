# Convert a glycoproteomics experiment to pseudo-glycomes

**\[experimental\]**

Converts a
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
into one
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
for each complete `(protein, protein_site)` pair in its row data.

## Usage

``` r
as_pseudo_glycomes(exp, aggr_method = c("sum", "mean", "median"))
```

## Arguments

- exp:

  A
  [`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md).

- aggr_method:

  Aggregation method to use. One of "sum", "mean", or "median". Default
  is "sum".

## Value

A named list of
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
objects. List names identify each glycosite as
`{protein}-{protein_site}`.
