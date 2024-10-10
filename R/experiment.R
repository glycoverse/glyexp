# There are some restrictions on the three data structures.
# 1. The `expr_mat` should be a matrix with samples as columns and variables as rows.
# 2. The `sample_info` should be a tibble::tibble with a column named "sample".
# 3. The `var_info` should be a tibble::tibble with a column named "variable".
# 4. `colnames(expr_mat)` should be identical to `sample_info$sample`,
#    order doesn't matter.
# 5. `rownames(expr_mat)` should be identical to `var_info$variable`,
#    order doesn't matter.
new_experiment <- function(name, expr_mat, sample_info, var_info) {
  stopifnot(is.character(name))
  stopifnot(is.matrix(expr_mat))
  stopifnot(tibble::is_tibble(sample_info))
  stopifnot(tibble::is_tibble(var_info))
  experiment <- list(
    name = name,
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info
  )
  class(experiment) <- "glyexp_experiment"
  return(experiment)
}


validate_experiment <- function(exp) {
  values <- unclass(exp)
  # Check if "sample" and "variable" columns are present in sample_info and var_info
  if (!"sample" %in% colnames(values$sample_info)) {
    stop("missing 'sample' column in `sample_info`", .call = FALSE)
  }
  if (!"variable" %in% colnames(values$var_info)) {
    stop("missing 'variable' column in `var_info`", .call = FALSE)
  }
  # Check if samples and variables are consistent
  if (!identical(colnames(values$expr_mat), values$sample_info$sample)) {
    stop("samples inn `sample_info` and `expr_mat` are different", .call = FALSE)
  }
  if (!identical(rownames(values$expr_mat), values$var_info$variable)) {
    stop("variables in `var_info` and `expr_mat` are different", .call = FALSE)
  }
  exp
}



