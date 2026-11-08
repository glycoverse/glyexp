# Package index

## Experiment Creation and Conversion

- [`GlycomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
  [`` `dimnames<-`( ``*`<GlycomicSE>`*`,`*`<list>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
  [`` `dimnames<-`( ``*`<GlycomicSE>`*`,`*`<NULL>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
  [`` `rownames<-`( ``*`<GlycomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
  [`` `colnames<-`( ``*`<GlycomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/GlycomicSE.md)
  : Create a GlycomicSE object
- [`GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
  [`` `dimnames<-`( ``*`<GlycoproteomicSE>`*`,`*`<list>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
  [`` `dimnames<-`( ``*`<GlycoproteomicSE>`*`,`*`<NULL>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
  [`` `rownames<-`( ``*`<GlycoproteomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
  [`` `colnames<-`( ``*`<GlycoproteomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/GlycoproteomicSE.md)
  : Create a GlycoproteomicSE object
- [`` `names<-`( ``*`<GlycomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `rowData<-`( ``*`<GlycomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `colData<-`( ``*`<GlycomicSE>`*`,`*`<DataFrame>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `colData<-`( ``*`<GlycomicSE>`*`,`*`<NULL>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `assays<-`( ``*`<GlycomicSE>`*`,`*`<list>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `assays<-`( ``*`<GlycomicSE>`*`,`*`<SimpleList>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `[`( ``*`<GlycomicSE>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `[<-`( ``*`<GlycomicSE>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<SummarizedExperiment>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`rbind(`*`<GlycomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`cbind(`*`<GlycomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `names<-`( ``*`<GlycoproteomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `rowData<-`( ``*`<GlycoproteomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `colData<-`( ``*`<GlycoproteomicSE>`*`,`*`<DataFrame>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `colData<-`( ``*`<GlycoproteomicSE>`*`,`*`<NULL>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `assays<-`( ``*`<GlycoproteomicSE>`*`,`*`<list>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `assays<-`( ``*`<GlycoproteomicSE>`*`,`*`<SimpleList>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `[`( ``*`<GlycoproteomicSE>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`` `[<-`( ``*`<GlycoproteomicSE>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<SummarizedExperiment>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`rbind(`*`<GlycoproteomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  [`cbind(`*`<GlycoproteomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/se-name-operations.md)
  : Keep glyco-omics names valid during SE operations
- [`as_tibble(`*`<GlycomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/as_tibble.GlycomicSE.md)
  [`as_tibble(`*`<GlycoproteomicSE>`*`)`](https://glycoverse.github.io/glyexp/dev/reference/as_tibble.GlycomicSE.md)
  : Convert a glycomics container to a long-format tibble
- [`as_glycomic_se()`](https://glycoverse.github.io/glyexp/dev/reference/as_glycomic_se.md)
  [`is_glycomic_se()`](https://glycoverse.github.io/glyexp/dev/reference/as_glycomic_se.md)
  : Coerce to GlycomicSE
- [`as_glycoproteomic_se()`](https://glycoverse.github.io/glyexp/dev/reference/as_glycoproteomic_se.md)
  [`is_glycoproteomic_se()`](https://glycoverse.github.io/glyexp/dev/reference/as_glycoproteomic_se.md)
  : Coerce to GlycoproteomicSE

## Experiment Inspection

- [`get_glycan_type()`](https://glycoverse.github.io/glyexp/dev/reference/get_glycan_type.md)
  : Get the glycan type of an experiment
- [`get_scale()`](https://glycoverse.github.io/glyexp/dev/reference/get_scale.md)
  : Get the abundance scale of an experiment
- [`set_scale()`](https://glycoverse.github.io/glyexp/dev/reference/set_scale.md)
  : Set the abundance scale of an experiment
- [`set_glycan_type()`](https://glycoverse.github.io/glyexp/dev/reference/set_glycan_type.md)
  : Set the glycan type of an experiment
- [`summarize_experiment()`](https://glycoverse.github.io/glyexp/dev/reference/summarize_experiment.md)
  : Identification overview

## Dplyr-Style Manipulation

- [`arrange_col()`](https://glycoverse.github.io/glyexp/dev/reference/arrange_col.md)
  [`arrange_row()`](https://glycoverse.github.io/glyexp/dev/reference/arrange_col.md)
  : Arrange sample or variable information
- [`filter_col()`](https://glycoverse.github.io/glyexp/dev/reference/filter_col.md)
  [`filter_row()`](https://glycoverse.github.io/glyexp/dev/reference/filter_col.md)
  : Filter samples or variables of an experiment
- [`left_join_col()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_col.md)
  [`inner_join_col()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_col.md)
  [`semi_join_col()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_col.md)
  [`anti_join_col()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_col.md)
  [`left_join_row()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_col.md)
  [`inner_join_row()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_col.md)
  [`semi_join_row()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_col.md)
  [`anti_join_row()`](https://glycoverse.github.io/glyexp/dev/reference/left_join_col.md)
  : Join data to sample or variable information
- [`mutate_col()`](https://glycoverse.github.io/glyexp/dev/reference/mutate_col.md)
  [`mutate_row()`](https://glycoverse.github.io/glyexp/dev/reference/mutate_col.md)
  : Mutate sample or variable information
- [`rename_col()`](https://glycoverse.github.io/glyexp/dev/reference/rename_col.md)
  [`rename_row()`](https://glycoverse.github.io/glyexp/dev/reference/rename_col.md)
  : Rename columns in the sample or variable information tibble
- [`select_col()`](https://glycoverse.github.io/glyexp/dev/reference/select_col.md)
  [`select_row()`](https://glycoverse.github.io/glyexp/dev/reference/select_col.md)
  : Select columns of the sample or variable information tibble
- [`slice_col()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  [`slice_row()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  [`slice_head_col()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  [`slice_head_row()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  [`slice_tail_col()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  [`slice_tail_row()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  [`slice_sample_col()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  [`slice_sample_row()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  [`slice_max_col()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  [`slice_max_row()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  [`slice_min_col()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  [`slice_min_row()`](https://glycoverse.github.io/glyexp/dev/reference/slice_col.md)
  : Slice sample or variable information

## Other Functions

- [`standardize_variable()`](https://glycoverse.github.io/glyexp/dev/reference/standardize_variable.md)
  : Standardize variable IDs in an experiment
- [`as_pseudo_glycome()`](https://glycoverse.github.io/glyexp/dev/reference/as_pseudo_glycome.md)
  : Convert a glycoproteomics experiment to a pseudo-glycome experiment
- [`as_pseudo_glycomes()`](https://glycoverse.github.io/glyexp/dev/reference/as_pseudo_glycomes.md)
  **\[experimental\]** : Convert a glycoproteomics experiment to
  pseudo-glycomes

## Datasets

- [`real_experiment`](https://glycoverse.github.io/glyexp/dev/reference/real_experiment.md)
  : Real glycoproteomics experiment
- [`real_experiment2`](https://glycoverse.github.io/glyexp/dev/reference/real_experiment2.md)
  : Real glycomics experiment
