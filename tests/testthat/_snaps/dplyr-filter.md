# filtering to no samples/variables raises an error

    Code
      filter_obs(exp, group == "bad")
    Condition
      Error in `filter_obs()`:
      ! No samples left after filtering.

---

    Code
      filter_var(exp, type == "bad")
    Condition
      Error in `filter_var()`:
      ! No variables left after filtering.

# filtering using non-existing columns raises an error

    Code
      filter_obs(exp, bad_column == 1)
    Condition
      Error in `filter_obs()`:
      ! Column bad_column not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      filter_var(exp, bad_column == 1)
    Condition
      Error in `filter_var()`:
      ! Column bad_column not found in `var_info`.
      i Available columns: variable and type

