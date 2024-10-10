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

