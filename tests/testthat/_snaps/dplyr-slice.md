# slice verbs require names for virtual identifiers

    Code
      slice_max_col(se, order_by = .sample, n = 1)
    Condition
      Error:
      ! Cannot use .sample because `colnames(exp)` does not exist.
      i Create it with `mutate_col(exp, .sample = ...)`.

---

    Code
      slice_min_row(se, order_by = .variable, n = 1)
    Condition
      Error:
      ! Cannot use .variable because `rownames(exp)` does not exist.
      i Create it with `mutate_row(exp, .variable = ...)`.

# named slice expressions do not match internal arguments

    Code
      slice_row(se, id = .variable)
    Condition
      Error:
      ! Arguments in `...` must be passed by position, not name. x Problematic argument: * id = .variable

# slice with non-existing columns raises an error

    Code
      slice_max_col(exp, order_by = bad_col, n = 1)
    Condition
      Error:
      ! Column bad_col not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      slice_min_row(exp, order_by = bad_col, n = 1)
    Condition
      Error:
      ! Column bad_col not found in `var_info`.
      i Available columns: variable and type

