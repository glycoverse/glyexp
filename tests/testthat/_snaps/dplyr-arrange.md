# arranging with non-existing columns raises an error

    Code
      arrange_col(exp, bad_col)
    Condition
      Error:
      ! Column bad_col not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      arrange_row(exp, bad_col)
    Condition
      Error:
      ! Column bad_col not found in `var_info`.
      i Available columns: variable and type

