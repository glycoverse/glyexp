# filtering using non-existing columns raises an error

    Code
      filter_col(exp, bad_column == 1)
    Condition
      Error:
      ! Column bad_column not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      filter_row(exp, bad_column == 1)
    Condition
      Error:
      ! Column bad_column not found in `var_info`.
      i Available columns: variable and type

