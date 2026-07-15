# mutate verbs handle missing SummarizedExperiment names

    Code
      mutate_col(se, copied = .sample)
    Condition
      Error:
      ! Cannot use .sample because `colnames(exp)` does not exist.
      i Create it with `mutate_col(exp, .sample = ...)`.

---

    Code
      mutate_row(se, copied = .variable)
    Condition
      Error:
      ! Cannot use .variable because `rownames(exp)` does not exist.
      i Create it with `mutate_row(exp, .variable = ...)`.

---

    Code
      mutate_row(se, id = .variable)
    Condition
      Error:
      ! Cannot use .variable because `rownames(exp)` does not exist.
      i Create it with `mutate_row(exp, .variable = ...)`.

---

    Code
      mutate_col(se, copied = .data$.sample)
    Condition
      Error:
      ! Cannot use .sample because `colnames(exp)` does not exist.
      i Create it with `mutate_col(exp, .sample = ...)`.

# mutating using non-existing columns raises an error

    Code
      mutate_col(exp, new_col = bad_col)
    Condition
      Error:
      ! Column bad_col not found in `sample_info`.
      i Available columns: sample and group

---

    Code
      mutate_row(exp, new_col = bad_col)
    Condition
      Error:
      ! Column bad_col not found in `var_info`.
      i Available columns: variable and type

# trying to mutate 'sample' with duplicated values raises an error

    Code
      mutate_col(exp, sample = 1)
    Condition
      Error:
      ! Column sample in `sample_info` must be unique.

# trying to mutate 'variable' with duplicated values raises an error

    Code
      mutate_row(exp, variable = 1)
    Condition
      Error:
      ! Column variable in `var_info` must be unique.

