# join verbs require names for virtual identifiers

    Code
      left_join_col(se, tibble::tibble(.sample = "S1"), by = ".sample")
    Condition
      Error:
      ! Cannot use .sample because `colnames(exp)` does not exist.
      i Create it with `mutate_col(exp, .sample = ...)`.

---

    Code
      left_join_row(se, tibble::tibble(.variable = "V1"), by = ".variable")
    Condition
      Error:
      ! Cannot use .variable because `rownames(exp)` does not exist.
      i Create it with `mutate_row(exp, .variable = ...)`.

---

    Code
      left_join_col(se, tibble::tibble(group = "A", .sample = "S1"), by = "group")
    Condition
      Error:
      ! Column .sample in `colData(exp)` is reserved for dimension names.
      i Choose a different metadata column name.

# relationship parameter is locked to many-to-one

    Code
      left_join_col(exp, extra_info, by = "sample", relationship = "one-to-one")
    Condition
      Error:
      ! The `relationship` parameter is locked to "many-to-one".
      i This ensures that the number of samples never increases, which would violate experiment object assumptions.

---

    Code
      inner_join_row(exp, extra_info, by = "variable", relationship = "one-to-many")
    Condition
      Error:
      ! The `relationship` parameter is locked to "many-to-one".
      i This ensures that the number of variables never increases, which would violate experiment object assumptions.

# join detects many-to-many relationships

    Code
      left_join_col(exp, extra_info, by = "sample")
    Condition
      Error:
      ! Each row in `x` must match at most 1 row in `y`. i Row 1 of `x` matches multiple rows in `y`.

# join with missing columns throws informative error

    Code
      left_join_col(exp, extra_info, by = "sample")
    Condition
      Error:
      ! Join columns in `y` must be present in the data. x Problem with `sample`.

