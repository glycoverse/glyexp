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
