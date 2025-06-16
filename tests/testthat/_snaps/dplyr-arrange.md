# arranging with non-existing columns raises an error

    Code
      arrange_samples(exp, bad_col)
    Condition
      Error in `arrange_samples()`:
      ! Column bad_col not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      arrange_variables(exp, bad_col)
    Condition
      Error in `arrange_variables()`:
      ! Column bad_col not found in `var_info`.
      i Available columns: variable and type

