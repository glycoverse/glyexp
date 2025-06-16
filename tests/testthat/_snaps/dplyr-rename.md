# trying to rename non-existing columns throws an error

    Code
      rename_obs(exp, new_group = group2)
    Condition
      Error in `rename_obs()`:
      ! Column group2 not found in `sample_info`.
      i Available columns: group

---

    Code
      rename_var(exp, new_type = type2)
    Condition
      Error in `rename_var()`:
      ! Column type2 not found in `var_info`.
      i Available columns: type

# trying to rename 'sample' or 'variable' columns throws an error

    Code
      rename_obs(exp, new_sample = sample)
    Condition
      Error in `rename_obs()`:
      ! You could not rename the "sample" column in `sample_info`.

---

    Code
      rename_var(exp, new_variable = variable)
    Condition
      Error in `rename_var()`:
      ! You could not rename the "variable" column in `var_info`.

