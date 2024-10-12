# mutating using non-existing columns raises an error

    Code
      mutate_samples(exp, new_col = bad_col)
    Condition
      Error in `mutate_samples()`:
      ! Column bad_col not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      mutate_variables(exp, new_col = bad_col)
    Condition
      Error in `mutate_variables()`:
      ! Column bad_col not found in `var_info`.
      i Available columns: variable and type

# trying to mutate 'sample' with duplicated values raises an error

    Code
      mutate_samples(exp, sample = 1)
    Condition
      Error in `mutate_samples()`:
      ! Column sample in `sample_info` must be unique.

# trying to mutate 'variable' with duplicated values raises an error

    Code
      mutate_variables(exp, variable = 1)
    Condition
      Error in `mutate_variables()`:
      ! Column variable in `var_info` must be unique.

