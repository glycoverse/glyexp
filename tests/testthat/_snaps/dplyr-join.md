# relationship parameter is locked to many-to-one

    Code
      left_join_obs(exp, extra_info, by = "sample", relationship = "one-to-one")
    Condition
      Error in `left_join_obs()`:
      ! The `relationship` parameter is locked to "many-to-one".
      i This ensures that the number of samples never increases, which would violate experiment object assumptions.

---

    Code
      inner_join_var(exp, extra_info, by = "variable", relationship = "one-to-many")
    Condition
      Error in `inner_join_var()`:
      ! The `relationship` parameter is locked to "many-to-one".
      i This ensures that the number of variables never increases, which would violate experiment object assumptions.

# join detects many-to-many relationships

    Code
      left_join_obs(exp, extra_info, by = "sample")
    Condition
      Error in `left_join_obs()`:
      ! Each row in `x` must match at most 1 row in `y`. i Row 1 of `x` matches multiple rows in `y`.

# join with no matching observations throws error

    Code
      inner_join_obs(exp, extra_info, by = "sample")
    Condition
      Error in `inner_join_obs()`:
      ! No samples left after join operation.

---

    Code
      semi_join_obs(exp, extra_info, by = "sample")
    Condition
      Error in `semi_join_obs()`:
      ! No samples left after join operation.

# join with missing columns throws informative error

    Code
      left_join_obs(exp, extra_info, by = "sample")
    Condition
      Error in `left_join_obs()`:
      ! Join columns in `y` must be present in the data. x Problem with `sample`.

