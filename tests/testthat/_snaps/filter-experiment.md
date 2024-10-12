# filtering to no samples/variables raises an error

    Code
      filter_samples(exp, group == "bad")
    Condition
      Error in `filter_samples()`:
      ! No samples left after filtering.

---

    Code
      filter_variables(exp, type == "bad")
    Condition
      Error in `filter_variables()`:
      ! No variables left after filtering.

# filtering using non-existing columns raises an error

    Code
      filter_samples(exp, bad_column == 1)
    Condition
      Error in `filter_samples()`:
      ! Column bad_column not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      filter_variables(exp, bad_column == 1)
    Condition
      Error in `filter_variables()`:
      ! Column bad_column not found in `var_info`.
      i Available columns: variable and type

