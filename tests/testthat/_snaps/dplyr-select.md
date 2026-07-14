# selecting 'sample' column raises an error

    Code
      select_col(exp, sample, col1)
    Condition
      Error:
      ! You should not explicitly select or deselect the "sample" column in `sample_info`.
      i The "sample" column will be handled by `select_col()` or `select_row()` automatically.

# selecting 'variable' column raises an error

    Code
      select_row(exp, variable, col1)
    Condition
      Error:
      ! You should not explicitly select or deselect the "variable" column in `var_info`.
      i The "variable" column will be handled by `select_col()` or `select_row()` automatically.

# selecting non-existing columns raises an error

    Code
      select_col(exp, bad_col)
    Condition
      Error:
      ! Column bad_col not found in `sample_info`.
      i Available columns: col1 and col2

---

    Code
      select_row(exp, bad_col)
    Condition
      Error:
      ! Column bad_col not found in `var_info`.
      i Available columns: col1 and col2

