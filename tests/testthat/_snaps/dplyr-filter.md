# filter verbs require names for virtual identifiers

    Code
      filter_col(se, .sample == "S1")
    Condition
      Error:
      ! Cannot use .sample because `colnames(exp)` does not exist.
      i Create it with `mutate_col(exp, .sample = ...)`.

---

    Code
      filter_row(se, .variable == "V1")
    Condition
      Error:
      ! Cannot use .variable because `rownames(exp)` does not exist.
      i Create it with `mutate_row(exp, .variable = ...)`.

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

