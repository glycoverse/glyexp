# select verbs require names for virtual identifiers

    Code
      select_col(se, .sample)
    Condition
      Error:
      ! Cannot use .sample because `colnames(exp)` does not exist.
      i Create it with `mutate_col(exp, .sample = ...)`.

---

    Code
      select_row(se, .variable)
    Condition
      Error:
      ! Cannot use .variable because `rownames(exp)` does not exist.
      i Create it with `mutate_row(exp, .variable = ...)`.

---

    Code
      select_col(se, dplyr::all_of(".sample"))
    Condition
      Error:
      ! Cannot use .sample because `colnames(exp)` does not exist.
      i Create it with `mutate_col(exp, .sample = ...)`.

---

    Code
      select_col(se, .sample = group)
    Condition
      Error:
      ! Column .sample in `sample_info` is reserved for dimension names.
      i Choose a different metadata column name.

---

    Code
      select_row(se, .variable = type)
    Condition
      Error:
      ! Column .variable in `var_info` is reserved for dimension names.
      i Choose a different metadata column name.

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

