library(glyread)
library(glyexp)
library(glyparse)
library(glyrepr)
library(tidyverse)
devtools::load_all()

real_experiment2 <- read_glyhunter("data-raw/real_g_data.csv")
sample_info <- read_csv("data-raw/real_g_sample_info.csv") |>
  select(-sample)
struc_df <- read_csv("data-raw/real_g_strucs.csv") |>
  mutate(glycan_structure = parse_glycoct(structure)) |>
  select(-structure) |>
  mutate(glycan_composition = as_glycan_composition(glycan_structure)) |>
  mutate(comp_str = as.character(convert_to_generic(glycan_composition))) |>
  select(comp_str, glycan_structure)

real_experiment2 <- real_experiment2 |>
  mutate_obs(sample = str_split_i(sample, "_", 3)) |>
  left_join_obs(sample_info, by = c("sample" = "maldi_pos")) |>
  mutate_obs(sample = paste0("S", row_number())) |>
  mutate_var(comp_str = as.character(glycan_composition)) |>
  inner_join_var(struc_df, by = "comp_str") |>
  mutate_var(variable = paste0("V", row_number())) |>
  mutate_var(glycan_composition = as_glycan_composition(glycan_structure)) |>
  select_var(glycan_composition, glycan_structure) |>
  filter_obs(group != "QC")

usethis::use_data(real_experiment2, overwrite = TRUE)
