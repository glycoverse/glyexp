# can't set exp_type and glycan_type through set_meta_data

    Code
      set_meta_data(exp, "exp_type", "glycoproteomics")
    Condition
      Error in `set_meta_data()`:
      ! Setting exp_type through `set_meta_data()` is unsafe.
      i Use `set_exp_type()` instead.

---

    Code
      set_meta_data(exp, "glycan_type", "O")
    Condition
      Error in `set_meta_data()`:
      ! Setting glycan_type through `set_meta_data()` is unsafe.
      i Use `set_glycan_type()` instead.

