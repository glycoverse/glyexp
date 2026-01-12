# Standardize variable IDs in an experiment

Converts meaningless variable IDs (like "GP1", "V1") into meaningful,
human-readable IDs based on the experiment type and available columns.

The format of the new IDs depends on the `exp_type` if `format` is not
specified:

- `glycomics`: `{glycan_composition}`, e.g., "Hex(5)HexNAc(2)"

- `glycoproteomics`: `{protein}-<site>-{glycan_composition}` or
  `{protein}-{glycan_composition}` if no `protein_site` column exists

- `traitomics`: `{motif}` or `{trait}` depending on which column is
  present

- `traitproteomics`: `{protein}-<site>-{motif}` or
  `{protein}-<site>-{trait}`

If duplicate IDs are generated (e.g., same composition with multiple
PSMs), a unique integer suffix is appended using the `unique_suffix`
pattern.

## Usage

``` r
standardize_variable(
  exp,
  format = NULL,
  unique_suffix = "-{N}",
  fasta = NULL,
  taxid = 9606
)
```

## Arguments

- exp:

  An
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

- format:

  A format string specifying how to construct variable IDs. Use
  `{column_name}` to insert values from `var_info` columns. For example,
  `"{gene}-{glycan_composition}"` would produce "GENE1-Hex(5)". If
  `NULL` (default), a sensible format is chosen based on `exp_type`. Use
  `<site>` to include the amino acid and position (e.g., "N32"). The
  `<site>` token is a special placeholder that gets replaced with
  `<aa><pos>` format (e.g., "N32", "S44"). It requires the
  `protein_site` column and uses the following decision tree to
  determine the amino acid:

  1.  If `peptide` and `peptide_site` columns exist, extract from
      peptide

  2.  Else if `fasta` is provided, extract from FASTA sequences

  3.  Else fetch from UniProt using `UniProt.ws`

- unique_suffix:

  A string pattern for making IDs unique when duplicates exist. Must
  contain `{N}` which will be replaced with the numeric suffix (1, 2,
  3...). Default is `"-{N}"` which produces IDs like "Hex(5)-1",
  "Hex(5)-2".

- fasta:

  Either a file path to a FASTA file or a named character vector with
  protein IDs as names and sequences as values. Used to look up amino
  acids for site representation when peptide columns are not available.
  Default: `NULL` (use UniProt.ws to fetch sequences).

- taxid:

  NCBI taxonomy ID for UniProt lookup. Default: `9606` (human).

## Value

The experiment with standardized variable IDs, invisibly.

## Examples
