# Compute site representation

This is the main function implementing the decision tree for computing
the amino acid and position representation.

## Usage

``` r
.compute_site_aa_pos(var_info, fasta = NULL, taxid = 9606)
```

## Arguments

- var_info:

  A tibble with protein, protein_site, and optionally peptide and
  peptide_site columns.

- fasta:

  Optional named character vector of protein sequences.

- taxid:

  UniProt taxonomy ID (default: 9606 for human).

## Value

A character vector of site representations.
