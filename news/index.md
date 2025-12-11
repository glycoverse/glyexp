# Changelog

## glyexp (development version)

## glyexp 0.10.3

### New features

- Add
  [`summarize_experiment()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md)
  to summarize an experiment.

### Minor improvements and bug fixes

- Fix input type checking in some functions.

## glyexp 0.10.2

### Minor improvements and fixes

- `glycan_type` argument of
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  and
  [`set_glycan_type()`](https://glycoverse.github.io/glyexp/reference/set_meta_data.md)
  now accepts more values, including “N”, “O-GalNAc”, “O-GlcNAc”,
  “O-Man”, “O-Fuc”, “O-Glc”, and NULL.

## glyexp 0.10.1

### Minor improvements and fixes

- glyexp now depends on the CRAN version of glyrepr.

## glyexp 0.10.0

This version is a big update for
[`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).
We have made this function more flexible, robust, and user-friendly. The
breaking changes we introduced in this version have a broad impact on
many glycoverse packages. So we recommend to update all packages to the
latest versions.

### Breaking Changes

- [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  now checks whether certain columns are present in `var_info`. For
  “glycomics” experiments, `glycan_composition` is required. For
  “glycoproteomics” experiments, `protein`, `protein_site`, and
  `glycan_composition` are required. If any required column is missing,
  an error will be raised.

### New Features

- [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  now intelligently coerce common column types to improve user
  experience. For example, it will convert `character` to `factor` for
  the “group” column in `sample_info`. This behavior can be controlled
  by the `coerce_col_types` argument.
- [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  now checks the column types for some important columns, such as
  `protein`, `protein_site`, `glycan_composition`, etc. The checking is
  performed after column coercion (if `coerce_col_types` is `TRUE`), and
  only a message is printed if some column type conventions are
  violated. This behavior can be controlled by the `check_col_types`
  argument.
- Add a new type of experiment: “others”. This type is for other omics
  experiments that are not glycoproteomics or glycomics. In this case,
  `glycan_type` can be `NULL`. Also, type coercion and checking are
  skipped for this type.
- Now the minimum required input for
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  is only an expression matrix. If not provided, `sample_info` and
  `var_info` will be automatically generated based on the column and row
  names of the expression matrix. `exp_type` will be set to “others” and
  `glycan_type` will be set to `NULL`. This gives users more flexibility
  to create
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  objects.
- “traitomics” and “traitproteomics” are valid values for `exp_type` of
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  now.

### Minor Improvements and Bug Fixes

- `toy_experiment` now has `exp_type` “others”.
- `real_experiment` and `real_experiment2` now have factors for the
  “group” column in `sample_info`.
- Update the “Creating Experiments” vignette to reflect the new changes.
- [`set_meta_data()`](https://glycoverse.github.io/glyexp/reference/set_meta_data.md),
  [`set_exp_type()`](https://glycoverse.github.io/glyexp/reference/set_meta_data.md),
  and
  [`set_glycan_type()`](https://glycoverse.github.io/glyexp/reference/set_meta_data.md)
  now check the meta data.
- [`get_meta_data()`](https://glycoverse.github.io/glyexp/reference/get_meta_data.md)
  now returns all meta data fields if `x` is `NULL`.

## glyexp 0.9.2

### Minor improvements and bug fixes

- Printing the
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  object now includes the experiment type in the title.
- Add a vignette to explain four experiment types.
- Add a vignette of creating an
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  object from scratch.

## glyexp 0.9.1

### Minor improvements and bug fixes

- Import `:=` from `rlang` to fix “could not find function”%\|\|%“” bug
  in [`as_se()`](https://glycoverse.github.io/glyexp/reference/as_se.md)
  and
  [`from_se()`](https://glycoverse.github.io/glyexp/reference/from_se.md)
  in some systems.

## glyexp 0.9.0

### New features

- Add `real_experiment2`, a glycomics example experiment.
- Add
  [`from_se()`](https://glycoverse.github.io/glyexp/reference/from_se.md)
  and
  [`as_se()`](https://glycoverse.github.io/glyexp/reference/as_se.md) to
  convert
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  to and from `SummarizedExperiment()` objects.
- Add [`split()`](https://rdrr.io/r/base/split.html) to split an
  experiment into a list of experiments.

### Minor improvements and bug fixes

- Add [`merge()`](https://rdrr.io/r/base/merge.html) and
  [`split()`](https://rdrr.io/r/base/split.html) to the getting started
  vignette.
- Add a picture to the getting started vignette to explain index
  columns.
- Fix a typo in `toy_experiment`: “N3N2” should be “H3N2”.
- Fix an error in the documentation of
  [`select_var()`](https://glycoverse.github.io/glyexp/reference/select_obs.md).

## glyexp 0.8.0

### Breaking changes

- `toy_experiment` is no longer a function, but a data object. Instead
  of using
  [`toy_experiment()`](https://glycoverse.github.io/glyexp/reference/toy_experiment.md)
  to get the example experiment, use `toy_experiment` directly.

### New features

- Add
  [`left_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md),
  [`left_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md),
  [`inner_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md),
  [`inner_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md),
  [`semi_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md),
  [`semi_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md),
  [`anti_join_obs()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md),
  and
  [`anti_join_var()`](https://glycoverse.github.io/glyexp/reference/left_join_obs.md).
  These functions are useful for adding new information to
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  from tibbles.
- Add a `real_experiment` data object. This is derived from a real-world
  N-glycoproteome dataset.

### Minor improvements and bug fixes

- Fix warnings in [`merge()`](https://rdrr.io/r/base/merge.html).
- Column types in `sample_info` and `var_info` are now displayed in the
  console when printing the
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md)
  object.
- [`set_exp_type()`](https://glycoverse.github.io/glyexp/reference/set_meta_data.md)
  and
  [`set_glycan_type()`](https://glycoverse.github.io/glyexp/reference/set_meta_data.md)
  now validate the input values.

## glyexp 0.7.0

### Major changes

- Add [`merge()`](https://rdrr.io/r/base/merge.html) function to merge
  two experiments.

### Minor improvements

- Update the documentation of
  [`experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.md).

## glyexp 0.6.0

### Major changes

- Add a series of functions to describe the experiment:
  - [`count_compositions()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md):
    The number of glycan compositions.
  - [`count_structures()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md):
    The number of glycan structures.
  - [`count_peptides()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md):
    The number of peptides.
  - [`count_glycopeptides()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md):
    The number of unique combinations of peptides, sites, and glycans.
  - [`count_glycoforms()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md):
    The number of unique combinations of proteins, sites, and glycans.
  - [`count_proteins()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md):
    The number of proteins.
  - [`count_glycosites()`](https://glycoverse.github.io/glyexp/reference/count_compositions.md):
    The number of unique combinations of proteins and sites.
- Remove `add_glycan_descriptions()`, `add_struc_descriptions()`, and
  `add_comp_descriptions()`. These functions are moved to the `glymotif`
  package.
