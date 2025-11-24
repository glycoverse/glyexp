# Identification overview

These functions are used to identify the number of glycan compositions,
glycan structures, glycopeptides, peptides, glycoforms, glycoproteins,
and glycosites in an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

## Usage

``` r
count_compositions(x)

count_structures(x)

count_peptides(x)

count_glycopeptides(x, count_struct = NULL)

count_glycoforms(x, count_struct = NULL)

count_proteins(x)

count_glycosites(x)
```

## Arguments

- x:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  object.

- count_struct:

  For `count_glycopeptides()` and `count_glycoforms()`, whether to count
  the number of glycan structures or glycopeptides. If `TRUE`,
  glycopeptides or glycoforms bearing different glycan structures with
  the same glycan composition are counted as different ones. If not
  provided (NULL), defaults to `TRUE` if `glycan_structure` column
  exists in the variable information tibble, otherwise `FALSE`.

## Value

An integer.

- `count_compositions()`: The number of glycan compositions.

- `count_structures()`: The number of glycan structures.

- `count_peptides()`: The number of peptides.

- `count_glycopeptides()`: The number of unique combinations of
  peptides, sites, and glycans.

- `count_glycoforms()`: The number of unique combinations of proteins,
  sites, and glycans.

- `count_proteins()`: The number of proteins.

- `count_glycosites()`: The number of unique combinations of proteins
  and sites.

## Details

The following columns are required in the variable information tibble:

- `count_compositions()`: `glycan_composition`

- `count_structures()`: `glycan_structure`

- `count_peptides()`: `peptide`

- `count_glycopeptides()`: `glycan_composition` or `glycan_structure`,
  `peptide`, `peptide_site`

- `count_glycoforms()`: `glycan_composition` or `glycan_structure`,
  `protein` or `proteins`, `protein_site` or `protein_sites`

- `count_proteins()`: `protein` or `proteins`

- `count_glycosites()`: `protein` or `proteins`, `protein_site` or
  `protein_sites`

You can use `count_struct` parameter to control how to count
glycopeptides and glycoforms, either by glycan structures or by glycan
compositions.

## Examples

``` r
exp <- real_experiment
count_compositions(exp)
#> [1] 477
count_structures(exp)
#> [1] 968
count_peptides(exp)
#> [1] 323
count_glycopeptides(exp)
#> [1] 4262
count_glycoforms(exp)
#> [1] 4001
count_proteins(exp)
#> [1] 162
count_glycosites(exp)
#> [1] 276
```
