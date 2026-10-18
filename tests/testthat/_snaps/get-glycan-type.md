# get_glycan_type warns for legacy experiments

    Code
      get_glycan_type(exp)
    Condition
      Warning:
      `get_glycan_type()` was deprecated in glyexp 0.16.0.
      i Use `GlycomicSE()` or `GlycoproteomicSE()` as the default data container.
    Output
      [1] "N"

# get_glycan_type rejects unsupported inputs

    Code
      get_glycan_type(NULL)
    Condition
      Error in `get_glycan_type()`:
      ! Assertion on 'exp' failed: Must inherit from class 'GlycomicSE'/'GlycoproteomicSE'/'glyexp_experiment', but has class 'NULL'.

---

    Code
      get_glycan_type(SummarizedExperiment::SummarizedExperiment())
    Condition
      Error in `get_glycan_type()`:
      ! Assertion on 'exp' failed: Must inherit from class 'GlycomicSE'/'GlycoproteomicSE'/'glyexp_experiment', but has class 'SummarizedExperiment'.

