# Real glycoproteomics experiment

A real glycoproteomics experiment object. This dataset is derived from
the unpublished ProteomeXchange dataset PXD063749. It contains serum
N-glycoproteome profiles from 12 patients with different liver
conditions: healthy (H), hepatitis (M), cirrhosis (Y), and
hepatocellular carcinoma (C). Glycopeptide identification and annotation
were performed using pGlyco3 (https://github.com/pFindStudio/pGlyco3),
and quantification was carried out with pGlycoQuant
(https://github.com/Power-Quant/pGlycoQuant). The raw data were imported
with `glyread::read_pglyco3_pglycoquant()`.

## Usage

``` r
real_experiment
```

## Format

An
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
object with 4262 variables and 12 samples.

### Variable information:

- `peptide`: peptide sequence

- `peptide_site`: site position on the peptide

- `protein`: protein accession

- `protein_site`: site position on the protein

- `gene`: gene symbol

- `glycan_composition`: glycan composition
  (glyrepr::glycan_composition())

- `glycan_structure`: glycan structure (glyrepr::glycan_structure())

### Sample information:

- `sample`: sample ID

- `group`: disease group, one of "H" (healthy), "M" (hepatitis), "Y"
  (cirrhosis), and "C" (hepatocellular carcinoma)
