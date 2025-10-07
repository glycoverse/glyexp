devtools::load_all()

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
  glycan_composition = c("H5N2", "H5N2", "H3N2", "H3N2")
)
meta_data <- list(exp_type = "others", glycan_type = "N")
toy_experiment <- new_experiment(expr_mat, sample_info, var_info, meta_data)

usethis::use_data(toy_experiment, overwrite = TRUE)
