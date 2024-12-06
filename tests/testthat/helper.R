create_expr_mat <- function(samples, variables) {
  n_row <- length(variables)
  n_col <- length(samples)
  mat <- matrix(1:(n_row*n_col), nrow = n_row)
  rownames(mat) <- variables
  colnames(mat) <- samples
  mat
}

create_sample_info <- function(samples) {
  tibble::tibble(
    sample = samples,
    group = rep("A", length(samples))
  )
}

create_var_info <- function(variables) {
  tibble::tibble(
    variable = variables,
    type = rep("B", length(variables))
  )
}

create_test_exp <- function(samples, variables) {
  expr_mat <- create_expr_mat(samples, variables)
  sample_info <- create_sample_info(samples)
  var_info <- create_var_info(variables)
  new_experiment("test_exp", expr_mat, sample_info, var_info, list())
}

create_test_exp_2 <- function() {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- tibble::tibble(
    sample = c("S1", "S2", "S3"),
    col1 = c(1, 2, 3),
    col2 = c("A", "B", "C")
  )
  var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    col1 = c(1, 2, 3),
    col2 = c("A", "B", "C")
  )
  new_experiment("my_exp", expr_mat, sample_info, var_info, list())
}
