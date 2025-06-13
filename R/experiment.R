#' Create a new experiment
#'
#' @description
#' An experiment is a S3 object that contains the data of a
#' glycoproteomics or glycomics experiment.
#' Expression matrix, sample information, and variable information
#' are required then will be managed by the experiment object.
#'
#' @details
#' `colnames(expr_mat)` should be identical to `sample_info$sample`,
#' and `rownames(expr_mat)` should be identical to `var_info$variable`.
#' Both "sample" and "variable" columns should be unique.
#' Order doesn't matter, as the expression matrix will be reordered
#' to match the order of `sample_info$sample` and `var_info$variable`.
#'
#' Other attributes can be added to the experiment object.
#' For example, `meta_data` can be used to store additional information
#' like experiment type ("N-glyproteomics", "O-glycoproteomics", "N-glycomics", etc.),
#' quantification method (TMT, iTRAQ, label-free, etc.), or any other information.
#' These two conventions are used in the `glyread` package.
#'
#' `experiment()` provides multiple methods in tidyverse style to
#' filter samples or variables and to add new sample information or
#' variable information.
#' It is the core of `glyexp` ecosystem as the data container.
#'
#' @param name A character string for the name of the experiment.
#' @param expr_mat An expression matrix with samples as columns and variables as rows.
#' @param sample_info A tibble with a column named "sample", and other
#'   columns other useful information about samples,
#'   e.g. group, batch, sex, age, etc.
#' @param var_info A tibble with a column named "variable", and other
#'   columns other useful information about variables,
#'   e.g. protein name, peptide, glycan composition, etc.
#' @param meta_data A list of additional information about the experiment.
#'
#' @returns A [experiment()]. If the input data is wrong, an error will be raised.
#'
#' @examples
#' expr_mat <- matrix(runif(9), nrow = 3, ncol = 3)
#' colnames(expr_mat) <- c("S1", "S2", "S3")
#' rownames(expr_mat) <- c("V1", "V2", "V3")
#' sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"), group = c("A", "B", "A"))
#' var_info <- tibble::tibble(variable = c("V1", "V2", "V3"), protein = c("P1", "P2", "P3"))
#' experiment("my_exp", expr_mat, sample_info, var_info)
#'
#' @export
experiment <- function(name, expr_mat, sample_info, var_info, meta_data = NULL) {
  # Coerce sample types
  expr_mat <- as.matrix(expr_mat)
  if (!tibble::is_tibble(sample_info)) {
    sample_info <- tibble::rownames_to_column(sample_info, "sample")
    sample_info <- tibble::as_tibble(sample_info)
  }
  if (!tibble::is_tibble(var_info)) {
    var_info <- tibble::rownames_to_column(var_info, "variable")
    var_info <- tibble::as_tibble(var_info)
  }

  # Check if "sample" and "variable" columns are present in sample_info and var_info
  if (!"sample" %in% colnames(sample_info)) {
    cli::cli_abort("`sample_info` must have a 'sample' column")
  }
  if (!"variable" %in% colnames(var_info)) {
    cli::cli_abort("`var_info` must have a 'variable' column")
  }

  # Check if samples are consistent
  if (!setequal(colnames(expr_mat), sample_info$sample)) {
    samples_not_right <- TRUE
    extra_samples <- setdiff(colnames(expr_mat), sample_info$sample)
    missing_samples <- setdiff(sample_info$sample, colnames(expr_mat))
    extra_sample_err_msg <- dplyr::if_else(
      length(extra_samples) > 0,
      "Samples in `expr_mat` but not in `sample_info`: {.val {extra_samples}}",
      ""
    )
    missing_sample_err_msg <- dplyr::if_else(
      length(missing_samples) > 0,
      "Samples in `sample_info` but not in `expr_mat`: {.val {missing_samples}}",
      ""
    )
    sample_err_msg <- stringr::str_c(extra_sample_err_msg, missing_sample_err_msg, sep = " ")
  } else {
    samples_not_right <- FALSE
    sample_err_msg <- ""
  }

  # Check if variables are consistent
  if (!setequal(rownames(expr_mat), var_info$variable)) {
    vars_not_right <- TRUE
    extra_vars <- setdiff(rownames(expr_mat), var_info$variable)
    missing_vars <- setdiff(var_info$variable, rownames(expr_mat))
    extra_var_err_msg <- dplyr::if_else(
      length(extra_vars) > 0,
      "Variables in `expr_mat` but not in `var_info`: {.val {extra_vars}}",
      ""
    )
    missing_var_err_msg <- dplyr::if_else(
      length(missing_vars) > 0,
      "Variables in `var_info` but not in `expr_mat`: {.val {missing_vars}}",
      ""
    )
    var_err_msg <- stringr::str_c(extra_var_err_msg, missing_var_err_msg, sep = " ")
  } else {
    vars_not_right <- FALSE
    var_err_msg <- ""
  }

  # Stop if samples or variables are not consistent
  if (samples_not_right || vars_not_right) {
    err_msg <- stringr::str_c(sample_err_msg, var_err_msg, sep = " ")
    cli::cli_abort(c(
      "Samples or variables must be consistent between `expr_mat`, `sample_info`, and `var_info`.",
      "x" = err_msg
    ))
  }

  # Check if samples and variables are unique
  samples_unique <- !anyDuplicated(sample_info$sample)
  vars_unique <- !anyDuplicated(var_info$variable)
  if (!samples_unique) {
    n_dup_samples <- sum(duplicated(sample_info$sample))
    sample_err_msg <- "{.val {n_dup_samples}} duplicated samples in `sample_info`."
  } else {
    sample_err_msg <- ""
  }
  if (!vars_unique) {
    n_dup_vars <- sum(duplicated(var_info$variable))
    var_err_msg <- "{.val {n_dup_vars}} duplicated variables in `var_info`."
  } else {
    var_err_msg <- ""
  }
  if (!samples_unique || !vars_unique) {
    cli::cli_abort(c(
      "Samples and variables must be unique.",
      "x" = stringr::str_c(sample_err_msg, var_err_msg, sep = " ")
    ))
  }

  # Reorder rows and columns of `expr_mat` to match `sample_info` and `var_info`
  expr_mat <- expr_mat[var_info$variable, sample_info$sample, drop = FALSE]

  if (is.null(meta_data)) {
    meta_data <- list()
  }

  new_experiment(name, expr_mat, sample_info, var_info, meta_data)
}


# There are some restrictions on the three data structures.
# 1. The `expr_mat` should be a matrix with samples as columns and variables as rows.
# 2. The `sample_info` should be a tibble::tibble with a column named "sample".
# 3. The `var_info` should be a tibble::tibble with a column named "variable".
# 4. `colnames(expr_mat)` should be identical to `sample_info$sample`,
#    order doesn't matter.
# 5. `rownames(expr_mat)` should be identical to `var_info$variable`,
#    order doesn't matter.
new_experiment <- function(name, expr_mat, sample_info, var_info, meta_data) {
  stopifnot(is.character(name))
  stopifnot(is.matrix(expr_mat))
  stopifnot(tibble::is_tibble(sample_info))
  stopifnot(tibble::is_tibble(var_info))
  experiment <- list(
    name = name,
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    meta_data = meta_data
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
  # Check if samples and variables are unique
  if (anyDuplicated(values$sample_info$sample)) {
    stop("duplicated 'sample' column in `sample_info`", .call = FALSE)
  }
  if (anyDuplicated(values$var_info$variable)) {
    stop("duplicated 'variable' column in `var_info`", .call = FALSE)
  }
  checkmate::assert_list(exp$meta_data)
  exp
}
