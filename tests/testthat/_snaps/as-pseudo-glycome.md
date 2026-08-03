# site-specific pseudo-glycome helpers validate their inputs

    Code
      as_pseudo_glycome(gp_se, glycosite = c(protein = "P1", protein_site = "1"))
    Condition
      Error in `.glycosite_rows()`:
      ! `glycosite` must be a named list with protein and protein_site.

---

    Code
      as_pseudo_glycome(gp_se, glycosite = list(protein = "P1", protein_site = 99L))
    Condition
      Error in `.glycosite_rows()`:
      ! No rows found for glycosite "P1-99".

---

    Code
      as_pseudo_glycome(create_test_gp_exp(), glycosite = list(protein = "P1",
        protein_site = 1L))
    Condition
      Error in `as_pseudo_glycome()`:
      ! `glycosite` is only supported for a <GlycoproteomicSE>.

---

    Code
      as_pseudo_glycomes(incomplete_gp_se)
    Condition
      Error in `.validate_complete_glycosites()`:
      ! `protein` and `protein_site` must be complete to split by glycosite.

---

    Code
      as_pseudo_glycomes(create_test_gp_exp())
    Condition
      Error in `as_pseudo_glycomes()`:
      ! `exp` must be a <GlycoproteomicSE>.
