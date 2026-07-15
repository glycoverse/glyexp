# arrange verbs require names for virtual identifiers

    Code
      arrange_col(se, .sample)
    Condition
      Error:
      ! Cannot use .sample because `colnames(exp)` does not exist.
      i Create it with `mutate_col(exp, .sample = ...)`.

---

    Code
      arrange_row(se, .variable)
    Condition
      Error:
      ! Cannot use .variable because `rownames(exp)` does not exist.
      i Create it with `mutate_row(exp, .variable = ...)`.

---

    Code
      arrange_row(se, id = .variable)
    Condition
      Error:
      ! Cannot use .variable because `rownames(exp)` does not exist.
      i Create it with `mutate_row(exp, .variable = ...)`.

# arranging with non-existing columns raises an error

    Code
      arrange_col(exp, bad_col)
    Condition
      Error:
      ! Column bad_col not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      arrange_row(exp, bad_col)
    Condition
      Error:
      ! Column bad_col not found in `var_info`.
      i Available columns: variable and type

