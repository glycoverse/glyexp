# creating experiment with missing samples in `sample_info`

    Code
      experiment("my_exp", expr_mat, sample_info, var_info)
    Condition
      Error in `experiment()`:
      ! Samples or variables must be consistent between `expr_mat`, `sample_info`, and `var_info`.
      x Samples in `expr_mat` but not in `sample_info`: "S3"

# creating experiment with missing variables in `var_info`

    Code
      experiment("my_exp", expr_mat, sample_info, var_info)
    Condition
      Error in `experiment()`:
      ! Samples or variables must be consistent between `expr_mat`, `sample_info`, and `var_info`.
      x  Variables in `expr_mat` but not in `var_info`: "V3"

# creating experiment with extra samples in `sample_info`

    Code
      experiment("my_exp", expr_mat, sample_info, var_info)
    Condition
      Error in `experiment()`:
      ! Samples or variables must be consistent between `expr_mat`, `sample_info`, and `var_info`.
      x  Samples in `sample_info` but not in `expr_mat`: "S4"

# creating experiment with extra variables in `var_info`

    Code
      experiment("my_exp", expr_mat, sample_info, var_info)
    Condition
      Error in `experiment()`:
      ! Samples or variables must be consistent between `expr_mat`, `sample_info`, and `var_info`.
      x  Variables in `var_info` but not in `expr_mat`: "V4"

# samples are not unique

    Code
      experiment("my_exp", expr_mat, sample_info, var_info)
    Condition
      Error in `experiment()`:
      ! Samples and variables must be unique.
      x 1 duplicated samples in `sample_info`.

# variables are not unique

    Code
      experiment("my_exp", expr_mat, sample_info, var_info)
    Condition
      Error in `experiment()`:
      ! Samples and variables must be unique.
      x  1 duplicated variables in `var_info`.

# both samples and variables are not unique

    Code
      experiment("my_exp", expr_mat, sample_info, var_info)
    Condition
      Error in `experiment()`:
      ! Samples and variables must be unique.
      x 1 duplicated samples in `sample_info`. 1 duplicated variables in `var_info`.

# only one sample still works

    Code
      experiment("my_exp", expr_mat, sample_info, var_info)
    Message
      
      -- Experiment ------------------------------------------------------------------
      i Name: "my_exp"
      i Expression matrix: 1 samples, 3 variables
      i Sample information fields: group
      i Variable information fields: type

# no variable works

    Code
      experiment("my_exp", expr_mat, sample_info, var_info)
    Message
      
      -- Experiment ------------------------------------------------------------------
      i Name: "my_exp"
      i Expression matrix: 3 samples, 0 variables
      i Sample information fields: 
      i Variable information fields: 

