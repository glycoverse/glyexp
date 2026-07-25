library(glyread)
library(glyexp)
library(glyparse)
library(glyrepr)
library(glyanno)
library(glydb)
library(tidyverse)
devtools::load_all()

real_experiment2 <- read_glyhunter("data-raw/real_g_data.csv")
sample_info <- read_csv("data-raw/real_g_sample_info.csv") |>
  select(-sample) |>
  mutate(group = factor(group, levels = c("H", "M", "Y", "C")))

real_experiment2 <- real_experiment2 |>
  mutate_col(.sample = str_split_i(.sample, "_", 3)) |>
  left_join_col(sample_info, by = c(".sample" = "maldi_pos")) |>
  mutate_row(glycan_structure = comp_to_struc(
    glycan_composition,
    db = glydb::glydb_structures("topological", species = "Homo sapiens", glycan_type = "N"),
    return_best = TRUE
  )) |>
  filter_row(!is.na(glycan_structure)) |>
  mutate_col(.sample = paste0("S", row_number())) |>
  mutate_row(.variable = paste0("V", row_number())) |>
  select_row(glycan_composition, glycan_structure) |>
  filter_col(group != "QC") |>
  standardize_variable()

usethis::use_data(real_experiment2, overwrite = TRUE)
