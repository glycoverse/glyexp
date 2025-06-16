# slice with non-existing columns raises an error

    Code
      slice_max_samples(exp, order_by = bad_col, n = 1)
    Condition
      Error in `slice_max_samples()`:
      ! Column bad_col not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      slice_min_variables(exp, order_by = bad_col, n = 1)
    Condition
      Error in `slice_min_variables()`:
      ! Column bad_col not found in `var_info`.
      i Available columns: variable and type

