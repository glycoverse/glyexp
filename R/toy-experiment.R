#' Create a Toy Experiment
#'
#' This function creates a toy experiment object for new users to play with
#' to get familiar with [experiment()] objects.
#' The experiment has 4 variables and 6 samples.
#' `var_info` contains fields: `protein`, `peptide`, and `glycan_composition`.
#' `sample_info` contains fields: `group` and `batch`.
#'
#' @return An [experiment()] object.
#' @export
toy_experiment <- function() {
  expr_mat <- matrix(1:24, nrow = 4)
  rownames(expr_mat) <- c("V1", "V2", "V3", "V4")
  colnames(expr_mat) <- c("S1", "S2", "S3", "S4", "S5", "S6")
  sample_info <- tibble::tibble(
    sample = c("S1", "S2", "S3", "S4", "S5", "S6"),
    group = c("A", "A", "A", "B", "B", "B"),
    batch = c(1, 2, 1, 2, 1, 2)
  )
  var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3", "V4"),
    protein = c("PRO1", "PRO2", "PRO3", "PRO3"),
    peptide = c("PEP1", "PEP2", "PEP3", "PEP4"),
    glycan_composition = c("H5N2", "H5N2", "N3N2", "N3N2")
  )
  meta_data <- list(exp_type = "glycoproteomics", glycan_type = "N")
  new_experiment(expr_mat, sample_info, var_info, meta_data)
}
