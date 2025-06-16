# arranging with non-existing columns raises an error

    Code
      arrange_obs(exp, bad_col)
    Condition
      Error in `arrange_obs()`:
      ! Column bad_col not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      arrange_var(exp, bad_col)
    Condition
      Error in `arrange_var()`:
      ! Column bad_col not found in `var_info`.
      i Available columns: variable and type

