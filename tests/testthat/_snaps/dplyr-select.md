# selecting 'sample' column raises an error

    Code
      select_samples(exp, sample, col1)
    Condition
      Error in `select_samples()`:
      ! You should not explicitly select or deselect the "sample" column in `sample_info`.
      i The "sample" column will be handled by `select_samples()` automatically.

# selecting 'variable' column raises an error

    Code
      select_variables(exp, variable, col1)
    Condition
      Error in `select_variables()`:
      ! You should not explicitly select or deselect the "variable" column in `var_info`.
      i The "variable" column will be handled by `select_variables()` automatically.

# selecting non-existing columns raises an error

    Code
      select_samples(exp, bad_col)
    Condition
      Error in `select_samples()`:
      ! Column bad_col not found in `sample_info`.
      i Available columns: col1 and col2

---

    Code
      select_variables(exp, bad_col)
    Condition
      Error in `select_variables()`:
      ! Column bad_col not found in `var_info`.
      i Available columns: col1 and col2

