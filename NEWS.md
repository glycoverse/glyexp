# glyexp 0.9.0

## New features

* Add `real_experiment2`, a glycomics example experiment.
* Add `from_se()` and `as_se()` to convert `experiment()` to and from `SummarizedExperiment()` objects.
* Add `split()` to split an experiment into a list of experiments.

## Minor improvements and bug fixes

* Add `merge()` and `split()` to the getting started vignette.
* Add a picture to the getting started vignette to explain index columns.
* Fix a typo in `toy_experiment`: "N3N2" should be "H3N2".
* Fix an error in the documentation of `select_var()`.

# glyexp 0.8.0

## Breaking changes

- `toy_experiment` is no longer a function, but a data object. Instead of using `toy_experiment()` to get the example experiment, use `toy_experiment` directly.

## New features

- Add `left_join_obs()`, `left_join_var()`, `inner_join_obs()`, `inner_join_var()`, `semi_join_obs()`, `semi_join_var()`, `anti_join_obs()`, and `anti_join_var()`. These functions are useful for adding new information to `experiment()` from tibbles.
- Add a `real_experiment` data object. This is derived from a real-world N-glycoproteome dataset.

## Minor improvements and bug fixes

- Fix warnings in `merge()`.
- Column types in `sample_info` and `var_info` are now displayed in the console when printing the `experiment()` object.
- `set_exp_type()` and `set_glycan_type()` now validate the input values.

# glyexp 0.7.0

## Major changes

- Add `merge()` function to merge two experiments.

## Minor improvements

- Update the documentation of `experiment()`.

# glyexp 0.6.0

## Major changes

- Add a series of functions to describe the experiment: 
    - `count_compositions()`: The number of glycan compositions.
    - `count_structures()`: The number of glycan structures.
    - `count_peptides()`: The number of peptides.
    - `count_glycopeptides()`: The number of unique combinations of peptides, sites, and glycans.
    - `count_glycoforms()`: The number of unique combinations of proteins, sites, and glycans.
    - `count_proteins()`: The number of proteins.
    - `count_glycosites()`: The number of unique combinations of proteins and sites.
- Remove `add_glycan_descriptions()`, `add_struc_descriptions()`, and `add_comp_descriptions()`.
  These functions are moved to the `glymotif` package.
