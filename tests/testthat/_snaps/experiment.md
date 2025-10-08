# creating experiment with missing samples in `sample_info`

    Code
      experiment(expr_mat, sample_info, var_info, "glycomics", "N")
    Condition
      Error in `experiment()`:
      ! Samples or variables must be consistent between `expr_mat`, `sample_info`, and `var_info`.
      x Samples in `expr_mat` but not in `sample_info`: S3

# creating experiment with missing variables in `var_info`

    Code
      experiment(expr_mat, sample_info, var_info, "glycoproteomics", "O")
    Condition
      Error in `experiment()`:
      ! Samples or variables must be consistent between `expr_mat`, `sample_info`, and `var_info`.
      x  Variables in `expr_mat` but not in `var_info`: V3

# creating experiment with extra samples in `sample_info`

    Code
      experiment(expr_mat, sample_info, var_info, "glycomics", "N")
    Condition
      Error in `experiment()`:
      ! Samples or variables must be consistent between `expr_mat`, `sample_info`, and `var_info`.
      x  Samples in `sample_info` but not in `expr_mat`: S4

# creating experiment with extra variables in `var_info`

    Code
      experiment(expr_mat, sample_info, var_info, "glycoproteomics", "O")
    Condition
      Error in `experiment()`:
      ! Samples or variables must be consistent between `expr_mat`, `sample_info`, and `var_info`.
      x  Variables in `var_info` but not in `expr_mat`: V4

# samples are not unique

    Code
      experiment(expr_mat, sample_info, var_info, "glycomics", "N")
    Condition
      Error in `experiment()`:
      ! Samples and variables must be unique.
      x 1 duplicated samples in `sample_info`.

# variables are not unique

    Code
      experiment(expr_mat, sample_info, var_info, "glycoproteomics", "O")
    Condition
      Error in `experiment()`:
      ! Samples and variables must be unique.
      x  1 duplicated variables in `var_info`.

# both samples and variables are not unique

    Code
      experiment(expr_mat, sample_info, var_info, "glycomics", "N")
    Condition
      Error in `experiment()`:
      ! Samples and variables must be unique.
      x 1 duplicated samples in `sample_info`. 1 duplicated variables in `var_info`.

# only one sample still works

    Code
      experiment(expr_mat, sample_info, var_info, "glycomics", "N")
    Message
      
      -- Glycomics Experiment --------------------------------------------------------
      i Expression matrix: 1 samples, 3 variables
      i Sample information fields: group <fct>
      i Variable information fields: glycan_composition <comp>

# no variable works

    Code
      experiment(expr_mat, sample_info, var_info, "glycomics", "N")
    Message
      
      -- Glycomics Experiment --------------------------------------------------------
      i Expression matrix: 3 samples, 0 variables
      i Sample information fields: none
      i Variable information fields: glycan_composition <comp>

# experiment validates exp_type parameter

    Code
      experiment(expr_mat, sample_info, var_info, "invalid_type", "N")
    Condition
      Error in `.check_meta_data()`:
      ! `exp_type` must be one of "glycomics", "glycoproteomics", "traitomics", "traitproteomics", or "others".
      x Got "invalid_type".

# experiment validates glycan_type parameter

    Code
      experiment(expr_mat, sample_info, var_info, "glycomics", "invalid_type")
    Condition
      Error in `.check_meta_data()`:
      ! `glycan_type` must be one of "N" or "O".
      x Got "invalid_type".

# experiment checks required columns in var_info

    Code
      experiment(expr_mat, sample_info, var_info, "glycomics", "N")
    Condition
      Error in `experiment()`:
      ! All required columns must be present in `var_info`.
      i Required columns: glycan_composition.
      x Missing columns: glycan_composition.

# experiment checks column types

    Code
      experiment(expr_mat, sample_info, var_info, "glycoproteomics", "N")
    Message
      ! Column protein_site contains non-integer numeric values; kept as <numeric>.
      ! Column protein_site should be <integer> instead of <numeric>.
      ! Column glycan_composition should be <glyrepr_composition> instead of <character>.
      i Some column type conventions are violated for var_info.
      i Consider correcting them and create a new experiment.
      
      -- Glycoproteomics Experiment --------------------------------------------------
      i Expression matrix: 3 samples, 3 variables
      i Sample information fields: group <fct>
      i Variable information fields: protein <chr>, protein_site <dbl>, glycan_composition <chr>

# experiment coerces common column types safely

    Code
      exp <- experiment(expr_mat, sample_info, var_info, "glycoproteomics", "N")
    Message
      Column group converted to <factor>.
      Column batch converted to <factor>.
      Column protein converted to <character>.
      Column protein_site converted to <integer>.
      ! Column peptide_site contains non-integer numeric values; kept as <numeric>.
      ! Column peptide_site should be <integer> instead of <numeric>.
      i Some column type conventions are violated for var_info.
      i Consider correcting them and create a new experiment.

# var_info cannot be NULL if exp_type is not others

    Code
      experiment(expr_mat, exp_type = "glycomics", glycan_type = "N")
    Condition
      Error in `.process_var_info()`:
      ! `var_info` must be provided if `exp_type` is not "others".
      x `exp_type` is "glycomics".

---

    Code
      experiment(expr_mat, exp_type = "glycoproteomics", glycan_type = "O")
    Condition
      Error in `.process_var_info()`:
      ! `var_info` must be provided if `exp_type` is not "others".
      x `exp_type` is "glycoproteomics".

