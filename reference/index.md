# Package index

## Experiment Creation and Conversion

- [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  [`is_experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  : Create a new experiment
- [`as_se()`](https://glycoverse.github.io/glyexp/reference/as_se.md) :
  Convert experiment to SummarizedExperiment
- [`from_se()`](https://glycoverse.github.io/glyexp/reference/from_se.md)
  : Convert SummarizedExperiment to experiment
- [`as_tibble(`*`<glyexp_experiment>`*`)`](https://glycoverse.github.io/glyexp/reference/as_tibble.glyexp_experiment.md)
  : Convert an experiment to a tibble

## Experiment Inspection

- [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  [`is_experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  : Create a new experiment
- [`get_expr_mat()`](https://glycoverse.github.io/glyexp/reference/get_expr_mat.md)
  : Get the expression matrix of an experiment
- [`get_sample_info()`](https://glycoverse.github.io/glyexp/reference/get_sample_info.md)
  : Get the sample information of an experiment
- [`get_var_info()`](https://glycoverse.github.io/glyexp/reference/get_var_info.md)
  : Get the variable information of an experiment
- [`get_meta_data()`](https://glycoverse.github.io/glyexp/reference/get_meta_data.md)
  [`get_exp_type()`](https://glycoverse.github.io/glyexp/reference/get_meta_data.md)
  [`get_glycan_type()`](https://glycoverse.github.io/glyexp/reference/get_meta_data.md)
  : Get the meta data of an experiment
- [`count_compositions()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md)
  [`count_structures()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md)
  [`count_peptides()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md)
  [`count_glycopeptides()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md)
  [`count_glycoforms()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md)
  [`count_proteins()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md)
  [`count_glycosites()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md)
  : Identification overview
- [`dim(`*`<glyexp_experiment>`*`)`](https://glycoverse.github.io/glyexp/reference/dim.glyexp_experiment.md)
  [`` `dim<-`( ``*`<glyexp_experiment>`*`)`](https://glycoverse.github.io/glyexp/reference/dim.glyexp_experiment.md)
  : Dimensions of an experiment
- [`dimnames(`*`<glyexp_experiment>`*`)`](https://glycoverse.github.io/glyexp/reference/dimnames.glyexp_experiment.md)
  : Dimname for experiment
- [`n_samples()`](https://glycoverse.github.io/glyexp/reference/n_samples.md)
  [`n_variables()`](https://glycoverse.github.io/glyexp/reference/n_samples.md)
  : Get number of samples or variables of an experiment
- [`samples()`](https://glycoverse.github.io/glyexp/reference/samples.md)
  [`variables()`](https://glycoverse.github.io/glyexp/reference/samples.md)
  : Get Samples or Variables of an Experiment

## Dplyr-Style Manipulation

- [`arrange_obs()`](https://glycoverse.github.io/glyexp/reference/arrange_obs.md)
  [`arrange_var()`](https://glycoverse.github.io/glyexp/reference/arrange_obs.md)
  : Arrange sample or variable information
- [`filter_obs()`](https://glycoverse.github.io/glyexp/reference/filter_obs.md)
  [`filter_var()`](https://glycoverse.github.io/glyexp/reference/filter_obs.md)
  : Filter samples or variables of an experiment
- [`left_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)
  [`inner_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)
  [`semi_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)
  [`anti_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)
  [`left_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)
  [`inner_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)
  [`semi_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)
  [`anti_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md)
  : Join data to sample or variable information
- [`mutate_obs()`](https://glycoverse.github.io/glyexp/reference/mutate_obs.md)
  [`mutate_var()`](https://glycoverse.github.io/glyexp/reference/mutate_obs.md)
  : Mutate sample or variable information
- [`rename_obs()`](https://glycoverse.github.io/glyexp/reference/rename_obs.md)
  [`rename_var()`](https://glycoverse.github.io/glyexp/reference/rename_obs.md)
  : Rename columns in the sample or variable information tibble
- [`select_obs()`](https://glycoverse.github.io/glyexp/reference/select_obs.md)
  [`select_var()`](https://glycoverse.github.io/glyexp/reference/select_obs.md)
  : Select columns of the sample or variable information tibble
- [`slice_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  [`slice_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  [`slice_head_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  [`slice_head_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  [`slice_tail_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  [`slice_tail_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  [`slice_sample_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  [`slice_sample_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  [`slice_max_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  [`slice_max_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  [`slice_min_obs()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  [`slice_min_var()`](https://glycoverse.github.io/glyexp/reference/slice_obs.md)
  : Slice sample or variable information

## Base-Style Manipulation

- [`merge(`*`<glyexp_experiment>`*`)`](https://glycoverse.github.io/glyexp/reference/merge.glyexp_experiment.md)
  : Merge two experiments
- [`split(`*`<glyexp_experiment>`*`)`](https://glycoverse.github.io/glyexp/reference/split.glyexp_experiment.md)
  : Split an experiment
- [`` `[`( ``*`<glyexp_experiment>`*`)`](https://glycoverse.github.io/glyexp/reference/sub-.glyexp_experiment.md)
  [`` `[<-`( ``*`<glyexp_experiment>`*`)`](https://glycoverse.github.io/glyexp/reference/sub-.glyexp_experiment.md)
  : Subsetting experiments

## Other Functions

- [`set_meta_data()`](https://glycoverse.github.io/glyexp/reference/set_meta_data.md)
  [`set_exp_type()`](https://glycoverse.github.io/glyexp/reference/set_meta_data.md)
  [`set_glycan_type()`](https://glycoverse.github.io/glyexp/reference/set_meta_data.md)
  : Set the meta data of an experiment

## Datasets

- [`toy_experiment`](https://glycoverse.github.io/glyexp/reference/toy_experiment.md)
  : Toy experiment
- [`real_experiment`](https://glycoverse.github.io/glyexp/reference/real_experiment.md)
  : Real glycoproteomics experiment
- [`real_experiment2`](https://glycoverse.github.io/glyexp/reference/real_experiment2.md)
  : Real glycomics experiment
