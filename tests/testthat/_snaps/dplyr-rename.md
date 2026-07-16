# rename verbs require names for virtual identifiers

    Code
      rename_col(se, id = .sample)
    Condition
      Error:
      ! Cannot use .sample because `colnames(exp)` does not exist.
      i Create it with `mutate_col(exp, .sample = ...)`.

---

    Code
      rename_row(se, id = .variable)
    Condition
      Error:
      ! Cannot use .variable because `rownames(exp)` does not exist.
      i Create it with `mutate_row(exp, .variable = ...)`.

---

    Code
      rename_col(se, .sample = group)
    Condition
      Error:
      ! Column .sample in `sample_info` is reserved for dimension names.
      i Choose a different metadata column name.

---

    Code
      rename_row(se, .variable = type)
    Condition
      Error:
      ! Column .variable in `var_info` is reserved for dimension names.
      i Choose a different metadata column name.

# trying to rename non-existing columns throws an error

    Code
      rename_col(exp, new_group = group2)
    Condition
      Error:
      ! Column group2 not found in `sample_info`.
      i Available columns: group

---

    Code
      rename_row(exp, new_type = type2)
    Condition
      Error:
      ! Column type2 not found in `var_info`.
      i Available columns: type

# trying to rename 'sample' or 'variable' columns throws an error

    Code
      rename_col(exp, new_sample = sample)
    Condition
      Error:
      ! You could not rename the "sample" column in `sample_info`.

---

    Code
      rename_row(exp, new_variable = variable)
    Condition
      Error:
      ! You could not rename the "variable" column in `var_info`.

