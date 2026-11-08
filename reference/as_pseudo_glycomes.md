# Convert a glycoproteomics experiment to pseudo-glycomes

**\[experimental\]**

Converts a
[`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.md)
into one
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
for each complete `(protein, protein_site)` pair in its row data.
Abundance scale handling follows
[`as_pseudo_glycome()`](https://glycoverse.github.io/glyexp/reference/as_pseudo_glycome.md).

## Usage

``` r
as_pseudo_glycomes(exp, aggr_method = c("sum", "mean", "median"))
```

## Arguments

- exp:

  A
  [`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.md).

- aggr_method:

  Aggregation method to use. One of "sum", "mean", or "median". Default
  is "sum".

## Value

A named list of
[`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
objects. List names identify each glycosite as
`{protein}-{protein_site}`.
