#' Toy experiment
#'
#' A toy experiment object for new users to play with
#' to get familiar with [experiment()] objects.
#'
#' @format ## `toy_experiment`
#' An [experiment()] object with 4 variables and 6 samples.
#' `var_info` contains fields: `protein`, `peptide`, and `glycan_composition`.
#' `sample_info` contains fields: `group` and `batch`.
"toy_experiment"

#' Real glycoproteomics experiment
#'
#' A real glycoproteomics experiment object.
#' This dataset is derived from the unpublished ProteomeXchange dataset PXD063749.
#' It contains serum N-glycoproteome profiles from 12 patients with different liver conditions:
#' healthy (H), hepatitis (M), cirrhosis (Y), and hepatocellular carcinoma (C).
#' Glycopeptide identification and annotation were performed using pGlyco3
#' (https://github.com/pFindStudio/pGlyco3), and quantification was carried out with pGlycoQuant
#' (https://github.com/Power-Quant/pGlycoQuant).
#' The raw data were imported with `glyread::read_pglyco3_pglycoquant()`.
#'
#' @format An [experiment()] object with 4262 variables and 12 samples.
#'
#' ## Variable information:
#' - `peptide`: peptide sequence
#' - `peptide_site`: site position on the peptide
#' - `protein`: protein accession
#' - `protein_site`: site position on the protein
#' - `gene`: gene symbol
#' - `glycan_composition`: glycan composition (glyrepr::glycan_composition())
#' - `glycan_structure`: glycan structure (glyrepr::glycan_structure())
#'
#' ## Sample information:
#' - `sample`: sample ID
#' - `group`: disease group, one of "H" (healthy), "M" (hepatitis), "Y" (cirrhosis), and "C" (hepatocellular carcinoma)
#'
"real_experiment"

#' Real glycomics experiment
#'
#' A real glycomics experiment object.
#' This dataset is derived from the unpublished glycoPOST dataset GPST000313.
#' It contains serum N-glycome profiles from 144 samples with different liver conditions:
#' healthy (H), hepatitis (M), cirrhosis (Y), and hepatocellular carcinoma (C).
#'
#' @format An [experiment()] object with 67 variables and 144 samples.
#'
#' ## Variable information:
#' - `glycan_composition`: glycan composition (glyrepr::glycan_composition())
#' - `glycan_structure`: glycan structure (glyrepr::glycan_structure())
#'
#' ## Sample information:
#' - `sample`: sample ID
#' - `group`: disease group, one of "H" (healthy), "M" (hepatitis), "Y" (cirrhosis), and "C" (hepatocellular carcinoma)
#'
"real_experiment2"