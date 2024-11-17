# subsetting with j omitted raises an error

    Code
      exp[1]
    Condition
      Error in `exp[1]`:
      ! Subsetting an experiment requires both row and column indices.

# [<- does not work

    Code
      exp[1, 1] <- 100
    Condition
      Error in `[<-`:
      ! Subsetting an experiment is read-only.

