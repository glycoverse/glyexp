# glyexp 0.12.3

## Minor improvements and bug fixes

* Fix `standardize_variable()` failing for many proteins due to UniProt API URL length limits. Queries are now batched to stay within limits.

# glyexp 0.12.2

## Minor improvements and bug fixes

* Fix UniProt.ws API usage to use the correct endpoint for mapping UniProt IDs to gene symbols.

# glyexp 0.12.1

## New features

* `standardize_variable()` now uses "X" for site when `protein_site` or `peptide_site` is NA.

# glyexp 0.12.0

## New features

* Add `standardize_variable()` to standardize the variable names of an experiment. This function makes variable names more human-readable and meaningful.

## Minor improvements and bug fixes

* `real_experiment` and `real_experiment2` now have standardized variable names.
* Fix a problem in `real_experiment` that the `peptide` column contains "J" instead of "N" for glycosites.

# glyexp 0.11.2

## Minor improvements and bug fixes

* `filter_obs()` and `filter_var()` now drop unused levels for columns used for filtering. For example, `filter_obs(exp, group %in% c("A", "B"))` will drop all other levels from the `group` column in `sample_info`, keeping only "A" and "B". This behavior makes downstream analysis easier.
* Fix a logic error in experiment type coercion. Previously, type coercion is performed as long as `check_col_types = TRUE`, even if `coerce_col_types = FALSE`. Now, type coercion is performed as long as `coerce_col_types = TRUE`.
* Update message of `experiment()` for type coercion and checking.
* Optimize the performance of `merge()`.
* Join functions now don't throw an error when no samples or variables are left after joining. Instead, they return an empty experiment. This change is to align with the behavior of `dplyr::join()`, which also returns an empty tibble when no rows are left after joining.
* Update documentation of `select_obs()` and `select_var()` to explain that the `sample` or `variable` column will always be kept.

# glyexp 0.11.1

## Breaking changes

* `filter_obs()` and `filter_var()` no longer throw an error when no samples or variables are left after filtering. Instead, they return an empty experiment. This change is to align with the behavior of `dplyr::filter()`, which also returns an empty tibble when no rows are left after filtering.

# glyexp 0.11.0

## Breaking changes

* Remove `count_xxx()` functions, as their functionality is now covered by `summarize_experiment()`.
* `summarize_experiment()` now count both total and per-sample numbers. For example, both the number of compositions identified in total (`total_composition`) and the average number of compositions detected per sample (`composition_per_sample`) are in the returned tibble.

# glyexp 0.10.4

## Minor improvements and bug fixes

* Fix a bug in `summarize_experiment()` that it fails when required columns are missing.

# glyexp 0.10.3

## New features

* Add `summarize_experiment()` to summarize an experiment.

## Minor improvements and bug fixes

* Fix input type checking in some functions.

# glyexp 0.10.2

## Minor improvements and fixes

* `glycan_type` argument of `experiment()` and `set_glycan_type()` now accepts more values, including "N", "O-GalNAc", "O-GlcNAc", "O-Man", "O-Fuc", "O-Glc", and NULL.

# glyexp 0.10.1

## Minor improvements and fixes

* glyexp now depends on the CRAN version of glyrepr.

# glyexp 0.10.0

This version is a big update for `experiment()`. We have made this function more flexible, robust, and user-friendly.
The breaking changes we introduced in this version have a broad impact on many glycoverse packages. So we recommend to update all packages to the latest versions.

## Breaking Changes

* `experiment()` now checks whether certain columns are present in `var_info`. For "glycomics" experiments, `glycan_composition` is required. For "glycoproteomics" experiments, `protein`, `protein_site`, and `glycan_composition` are required. If any required column is missing, an error will be raised.

## New Features

* `experiment()` now intelligently coerce common column types to improve user experience. For example, it will convert `character` to `factor` for the "group" column in `sample_info`. This behavior can be controlled by the `coerce_col_types` argument.
* `experiment()` now checks the column types for some important columns, such as `protein`, `protein_site`, `glycan_composition`, etc. The checking is performed after column coercion (if `coerce_col_types` is `TRUE`), and only a message is printed if some column type conventions are violated. This behavior can be controlled by the `check_col_types` argument.
* Add a new type of experiment: "others". This type is for other omics experiments that are not glycoproteomics or glycomics. In this case, `glycan_type` can be `NULL`. Also, type coercion and checking are skipped for this type.
* Now the minimum required input for `experiment()` is only an expression matrix. If not provided, `sample_info` and `var_info` will be automatically generated based on the column and row names of the expression matrix. `exp_type` will be set to "others" and `glycan_type` will be set to `NULL`. This gives users more flexibility to create `experiment()` objects.
* "traitomics" and "traitproteomics" are valid values for `exp_type` of `experiment()` now.

## Minor Improvements and Bug Fixes

* `toy_experiment` now has `exp_type` "others".
* `real_experiment` and `real_experiment2` now have factors for the "group" column in `sample_info`.
* Update the "Creating Experiments" vignette to reflect the new changes.
* `set_meta_data()`, `set_exp_type()`, and `set_glycan_type()` now check the meta data.
* `get_meta_data()` now returns all meta data fields if `x` is `NULL`.

# glyexp 0.9.2

## Minor improvements and bug fixes

* Printing the `experiment()` object now includes the experiment type in the title.
* Add a vignette to explain four experiment types.
* Add a vignette of creating an `experiment()` object from scratch.

# glyexp 0.9.1

## Minor improvements and bug fixes

* Import `:=` from `rlang` to fix "could not find function "%||%"" bug in `as_se()` and `from_se()` in some systems.

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
