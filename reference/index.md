# Package index

## Experiment Creation and Conversion

- [`GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.md)
  **\[experimental\]** : Create a GlycomicSE object
- [`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.md)
  **\[experimental\]** : Create a GlycoproteomicSE object
- [`as_glycomic_se()`](https://glycoverse.github.io/glyexp/reference/as_glycomic_se.md)
  [`is_glycomic_se()`](https://glycoverse.github.io/glyexp/reference/as_glycomic_se.md)
  : Coerce to GlycomicSE
- [`as_glycoproteomic_se()`](https://glycoverse.github.io/glyexp/reference/as_glycoproteomic_se.md)
  [`is_glycoproteomic_se()`](https://glycoverse.github.io/glyexp/reference/as_glycoproteomic_se.md)
  : Coerce to GlycoproteomicSE

## Experiment Inspection

- [`summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/summarize_experiment.md)
  : Identification overview

## Dplyr-Style Manipulation

- [`arrange_col()`](https://glycoverse.github.io/glyexp/reference/arrange_col.md)
  [`arrange_row()`](https://glycoverse.github.io/glyexp/reference/arrange_col.md)
  : Arrange sample or variable information
- [`filter_col()`](https://glycoverse.github.io/glyexp/reference/filter_col.md)
  [`filter_row()`](https://glycoverse.github.io/glyexp/reference/filter_col.md)
  : Filter samples or variables of an experiment
- [`left_join_col()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md)
  [`inner_join_col()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md)
  [`semi_join_col()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md)
  [`anti_join_col()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md)
  [`left_join_row()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md)
  [`inner_join_row()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md)
  [`semi_join_row()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md)
  [`anti_join_row()`](https://glycoverse.github.io/glyexp/reference/left_join_col.md)
  : Join data to sample or variable information
- [`mutate_col()`](https://glycoverse.github.io/glyexp/reference/mutate_col.md)
  [`mutate_row()`](https://glycoverse.github.io/glyexp/reference/mutate_col.md)
  : Mutate sample or variable information
- [`rename_col()`](https://glycoverse.github.io/glyexp/reference/rename_col.md)
  [`rename_row()`](https://glycoverse.github.io/glyexp/reference/rename_col.md)
  : Rename columns in the sample or variable information tibble
- [`select_col()`](https://glycoverse.github.io/glyexp/reference/select_col.md)
  [`select_row()`](https://glycoverse.github.io/glyexp/reference/select_col.md)
  : Select columns of the sample or variable information tibble
- [`slice_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  [`slice_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  [`slice_head_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  [`slice_head_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  [`slice_tail_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  [`slice_tail_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  [`slice_sample_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  [`slice_sample_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  [`slice_max_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  [`slice_max_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  [`slice_min_col()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  [`slice_min_row()`](https://glycoverse.github.io/glyexp/reference/slice_col.md)
  : Slice sample or variable information

## Other Functions

- [`standardize_variable()`](https://glycoverse.github.io/glyexp/reference/standardize_variable.md)
  : Standardize variable IDs in an experiment
- [`as_pseudo_glycome()`](https://glycoverse.github.io/glyexp/reference/as_pseudo_glycome.md)
  : Convert a glycoproteomics experiment to a pseudo-glycome experiment

## Datasets

- [`real_experiment`](https://glycoverse.github.io/glyexp/reference/real_experiment.md)
  : Real glycoproteomics experiment
- [`real_experiment2`](https://glycoverse.github.io/glyexp/reference/real_experiment2.md)
  : Real glycomics experiment
