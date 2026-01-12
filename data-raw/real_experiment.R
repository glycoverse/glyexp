library(glyread)
library(stringr)
devtools::load_all()

real_experiment <- read_pglyco3_pglycoquant("data-raw/real_gp_data.list", parse_structure = TRUE) |>
  mutate_obs(
    sample = str_split_i(sample, "-", -1L),
    sample = str_replace(sample, "_", ""),
    group = factor(str_sub(sample, 1L, 1L), levels = c("H", "M", "Y", "C"))
  ) |>
  mutate_var(peptide = str_replace_all(peptide, "J", "N")) |>
  standardize_variable()
usethis::use_data(real_experiment, overwrite = TRUE)
