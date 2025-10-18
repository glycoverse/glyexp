# mutating using non-existing columns raises an error

    Code
      mutate_obs(exp, new_col = bad_col)
    Condition
      Error:
      ! Column bad_col not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      mutate_var(exp, new_col = bad_col)
    Condition
      Error:
      ! Column bad_col not found in `var_info`.
      i Available columns: variable and type

# trying to mutate 'sample' with duplicated values raises an error

    Code
      mutate_obs(exp, sample = 1)
    Condition
      Error:
      ! Column sample in `sample_info` must be unique.

# trying to mutate 'variable' with duplicated values raises an error

    Code
      mutate_var(exp, variable = 1)
    Condition
      Error:
      ! Column variable in `var_info` must be unique.

