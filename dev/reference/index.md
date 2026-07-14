# Package index

## Experiment Creation and Conversion

- [`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
  **\[experimental\]** : Create a GlycomicSE object
- [`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
  **\[experimental\]** : Create a GlycoproteomicSE object
- [`as_glycomic_se()`](https://glycoverse.github.io/glyexp/dev/reference/as_glycomic_se.md)
  [`is_glycomic_se()`](https://glycoverse.github.io/glyexp/dev/reference/as_glycomic_se.md)
  : Coerce to GlycomicSE
- [`as_glycoproteomic_se()`](https://glycoverse.github.io/glyexp/dev/reference/as_glycoproteomic_se.md)
  [`is_glycoproteomic_se()`](https://glycoverse.github.io/glyexp/dev/reference/as_glycoproteomic_se.md)
  : Coerce to GlycoproteomicSE

## Experiment Inspection

- [`summarize_experiment()`](https://glycoverse.github.io/glyexp/dev/reference/summarize_experiment.md)
  : Identification overview

## Dplyr-Style Manipulation

- [`arrange_obs()`](https://glycoverse.github.io/glyexp/dev/reference/arrange_obs.md)
  [`arrange_var()`](https://glycoverse.github.io/glyexp/dev/reference/arrange_obs.md)
  : Arrange sample or variable information
- [`filter_obs()`](https://glycoverse.github.io/glyexp/dev/reference/filter_obs.md)
  [`filter_var()`](https://glycoverse.github.io/glyexp/dev/reference/filter_obs.md)
  : Filter samples or variables of an experiment
- [`left_join_obs()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)
  [`inner_join_obs()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)
  [`semi_join_obs()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)
  [`anti_join_obs()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)
  [`left_join_var()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)
  [`inner_join_var()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)
  [`semi_join_var()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)
  [`anti_join_var()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_obs.md)
  : Join data to sample or variable information
- [`mutate_obs()`](https://glycoverse.github.io/glyexp/dev/reference/mutate_obs.md)
  [`mutate_var()`](https://glycoverse.github.io/glyexp/dev/reference/mutate_obs.md)
  : Mutate sample or variable information
- [`rename_obs()`](https://glycoverse.github.io/glyexp/dev/reference/rename_obs.md)
  [`rename_var()`](https://glycoverse.github.io/glyexp/dev/reference/rename_obs.md)
  : Rename columns in the sample or variable information tibble
- [`select_obs()`](https://glycoverse.github.io/glyexp/dev/reference/select_obs.md)
  [`select_var()`](https://glycoverse.github.io/glyexp/dev/reference/select_obs.md)
  : Select columns of the sample or variable information tibble
- [`slice_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  [`slice_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  [`slice_head_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  [`slice_head_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  [`slice_tail_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  [`slice_tail_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  [`slice_sample_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  [`slice_sample_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  [`slice_max_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  [`slice_max_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  [`slice_min_obs()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  [`slice_min_var()`](https://glycoverse.github.io/glyexp/dev/reference/slice_obs.md)
  : Slice sample or variable information

## Other Functions

- [`standardize_variable()`](https://glycoverse.github.io/glyexp/dev/reference/standardize_variable.md)
  : Standardize variable IDs in an experiment
- [`as_pseudo_glycome()`](https://glycoverse.github.io/glyexp/dev/reference/as_pseudo_glycome.md)
  : Convert a glycoproteomics experiment to a pseudo-glycome experiment

## Datasets

- [`real_experiment`](https://glycoverse.github.io/glyexp/dev/reference/real_experiment.md)
  : Real glycoproteomics experiment
- [`real_experiment2`](https://glycoverse.github.io/glyexp/dev/reference/real_experiment2.md)
  : Real glycomics experiment
