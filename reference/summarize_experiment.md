# Identification overview

This function summarizes the number of glycan compositions, glycan
structures, glycopeptides, peptides, glycoforms, glycoproteins, and
glycosites in an
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

## Usage

``` r
summarize_experiment(x, count_struct = NULL)
```

## Arguments

- x:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  object.

- count_struct:

  For counting glycopeptides and glycoforms. whether to count the number
  of glycan structures or glycopeptides. If `TRUE`, glycopeptides or
  glycoforms bearing different glycan structures with the same glycan
  composition are counted as different ones. If not provided (NULL),
  defaults to `TRUE` if `glycan_structure` column exists in the variable
  information tibble, otherwise `FALSE`.

## Value

A tibble with columns `item` and `n` summarizing the results. The items
include:

- `total_composition`: The number of glycan compositions.

- `total_structure`: The number of glycan structures.

- `total_peptide`: The number of peptides.

- `total_glycopeptide`: The number of unique combinations of peptides,
  sites, and glycans.

- `total_glycoform`: The number of unique combinations of proteins,
  sites, and glycans.

- `total_protein`: The number of proteins.

- `total_glycosite`: The number of unique combinations of proteins and
  sites.

In addition, `_per_sample` items are calculated as the average number of
detected items per sample. For example, `composition_per_sample` is the
average number of glycan compositions detected per sample.

## Details

The following columns are required in the variable information tibble:

- `composition`: `glycan_composition`

- `structure`: `glycan_structure`

- `peptide`: `peptide`

- `glycopeptide`: `glycan_composition` or `glycan_structure`, `peptide`,
  `peptide_site`

- `glycoform`: `glycan_composition` or `glycan_structure`, `protein` or
  `proteins`, `protein_site` or `protein_sites`

- `protein`: `protein` or `proteins`

- `glycosite`: `protein` or `proteins`, `protein_site` or
  `protein_sites`

You can use `count_struct` parameter to control how to count
glycopeptides and glycoforms, either by glycan structures or by glycan
compositions.

## Examples

``` r
exp <- real_experiment
summarize_experiment(exp)
#> # A tibble: 14 × 2
#>    item                        n
#>    <chr>                   <dbl>
#>  1 total_composition        477 
#>  2 total_structure          968 
#>  3 total_peptide            323 
#>  4 total_glycopeptide      4262 
#>  5 total_glycoform         4001 
#>  6 total_protein            162 
#>  7 total_glycosite          276 
#>  8 composition_per_sample   460.
#>  9 structure_per_sample     924.
#> 10 peptide_per_sample       311 
#> 11 glycopeptide_per_sample 3923.
#> 12 glycoform_per_sample    3705 
#> 13 protein_per_sample       156.
#> 14 glycosite_per_sample     268.
```
