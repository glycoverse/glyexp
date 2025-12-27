# filtering using non-existing columns raises an error

    Code
      filter_obs(exp, bad_column == 1)
    Condition
      Error:
      ! Column bad_column not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      filter_var(exp, bad_column == 1)
    Condition
      Error:
      ! Column bad_column not found in `var_info`.
      i Available columns: variable and type

