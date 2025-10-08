# set_meta_data checks meta data

    Code
      set_meta_data(exp, "exp_type", "invalid_type")
    Condition
      Error in `.check_meta_data()`:
      ! `exp_type` must be one of "glycomics", "glycoproteomics", "traitomics", "traitproteomics", or "others".
      x Got "invalid_type".

---

    Code
      set_meta_data(exp, "glycan_type", "invalid_type")
    Condition
      Error in `.check_meta_data()`:
      ! `glycan_type` must be one of "N" or "O".
      x Got "invalid_type".

