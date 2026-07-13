# legacy experiment APIs recommend the default SE containers

    Code
      invisible(experiment(matrix(1, 1, 1, dimnames = list("V1", "S1"))))
    Condition
      Warning:
      `experiment()` was deprecated in glyexp 0.16.0.
      i Use `GlycomicSE()` or `GlycoproteomicSE()` as the default data container.

# read-only legacy methods warn before retaining their errors

    Code
      dim(exp) <- c(2, 2)
    Condition
      Warning:
      Using `dim<-` on a glyexp_experiment object was deprecated in glyexp 0.16.0.
      i Use `GlycomicSE()` or `GlycoproteomicSE()` as the default data container.
      Error in `dim<-`:
      ! Dimensions of an experiment could not be set manually.

---

    Code
      exp[1, 1] <- 0
    Condition
      Warning:
      Using `[<-` on a glyexp_experiment object was deprecated in glyexp 0.16.0.
      i Use `GlycomicSE()` or `GlycoproteomicSE()` as the default data container.
      Error in `[<-`:
      ! Subsetting an experiment is read-only.

