# selecting 'sample' column raises an error

    Code
      select_obs(exp, sample, col1)
    Condition
      Error in `select_obs()`:
      ! You should not explicitly select or deselect the "sample" column in `sample_info`.
      i The "sample" column will be handled by `select_obs()` automatically.

# selecting 'variable' column raises an error

    Code
      select_var(exp, variable, col1)
    Condition
      Error in `select_var()`:
      ! You should not explicitly select or deselect the "variable" column in `var_info`.
      i The "variable" column will be handled by `select_var()` automatically.

# selecting non-existing columns raises an error

    Code
      select_obs(exp, bad_col)
    Condition
      Error in `select_obs()`:
      ! Column bad_col not found in `sample_info`.
      i Available columns: col1 and col2

---

    Code
      select_var(exp, bad_col)
    Condition
      Error in `select_var()`:
      ! Column bad_col not found in `var_info`.
      i Available columns: col1 and col2

