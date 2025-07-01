#' Create a new experiment
#'
#' @description
#' The data container of a glycoproteomics or glycomics experiment.
#' Expression matrix, sample information, and variable information
#' are required then will be managed by the experiment object.
#' It acts as the data core of the `glycoverse` ecosystem.
#'
#' The `glyexp` package provides a set of functions to create,
#' manipulate, and analyze [experiment()] objects in a tidyverse style.
#'
#' @details
#' 
#' ## Requirements of the input data
#' 
#' `colnames(expr_mat)` should be identical to `sample_info$sample`,
#' and `rownames(expr_mat)` should be identical to `var_info$variable`.
#' Both "sample" and "variable" columns should be unique.
#' Order doesn't matter, as the expression matrix will be reordered
#' to match the order of `sample_info$sample` and `var_info$variable`.
#' 
#' ## Meta data
#'
#' Other meta data can be added to the `meta_data` attribute.
#' `meta_data` is a list of additional information about the experiment.
#' Two meta data fields are required:
#'
#' - `exp_type`: "glycomics" or "glycoproteomics"
#' - `glycan_type`: "N" or "O"
#'
#' Other meta data will be added by other `glycoverse` packages for their own purposes.
#'
#' @param expr_mat An expression matrix with samples as columns and variables as rows.
#' @param sample_info A tibble with a column named "sample", and other
#'   columns other useful information about samples,
#'   e.g. group, batch, sex, age, etc.
#' @param var_info A tibble with a column named "variable", and other
#'   columns other useful information about variables,
#'   e.g. protein name, peptide, glycan composition, etc.
#' @param exp_type The type of the experiment, "glycomics" or "glycoproteomics".
#' @param glycan_type The type of glycan, "N" or "O".
#' @param ... Other meta data about the experiment.
#'
#' @returns A [experiment()]. If the input data is wrong, an error will be raised.
#'
#' @examples
#' expr_mat <- matrix(runif(9), nrow = 3, ncol = 3)
#' colnames(expr_mat) <- c("S1", "S2", "S3")
#' rownames(expr_mat) <- c("V1", "V2", "V3")
#' sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"), group = c("A", "B", "A"))
#' var_info <- tibble::tibble(variable = c("V1", "V2", "V3"), protein = c("P1", "P2", "P3"))
#' experiment(
#'   expr_mat, sample_info, var_info,
#'   exp_type = "glycoproteomics",
#'   glycan_type = "N"
#' )
#'
#' @export
experiment <- function(expr_mat, sample_info, var_info, exp_type, glycan_type, ...) {
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
  checkmate::assert_choice(exp_type, c("glycomics", "glycoproteomics"))
  checkmate::assert_choice(glycan_type, c("N", "O"))

  # Check if "sample" and "variable" columns are present in sample_info and var_info
  if (!"sample" %in% colnames(sample_info)) {
    cli::cli_abort("`sample_info` must have a 'sample' column")
  }
  if (!"variable" %in% colnames(var_info)) {
    cli::cli_abort("`var_info` must have a 'variable' column")
  }

  # Check consistency between expression matrix and info tables
  check_consistency <- function(expr_names, info_names, expr_label, info_label) {
    if (!setequal(expr_names, info_names)) {
      extra_items <- setdiff(expr_names, info_names)
      missing_items <- setdiff(info_names, expr_names)
      extra_err_msg <- dplyr::if_else(
        length(extra_items) > 0,
        paste0(expr_label, " in `expr_mat` but not in `", info_label, "`: ", paste(extra_items, collapse = ", ")),
        ""
      )
      missing_err_msg <- dplyr::if_else(
        length(missing_items) > 0,
        paste0(expr_label, " in `", info_label, "` but not in `expr_mat`: ", paste(missing_items, collapse = ", ")),
        ""
      )
      err_msg <- stringr::str_c(extra_err_msg, missing_err_msg, sep = " ")
      return(list(consistent = FALSE, error_msg = err_msg))
    } else {
      return(list(consistent = TRUE, error_msg = ""))
    }
  }

  sample_check <- check_consistency(
    colnames(expr_mat), sample_info$sample, 
    "Samples", "sample_info"
  )
  var_check <- check_consistency(
    rownames(expr_mat), var_info$variable, 
    "Variables", "var_info"
  )

  # Stop if samples or variables are not consistent
  if (!sample_check$consistent || !var_check$consistent) {
    err_msg <- stringr::str_c(sample_check$error_msg, var_check$error_msg, sep = " ")
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

  meta_data <- list(exp_type = exp_type, glycan_type = glycan_type, ...)

  new_experiment(expr_mat, sample_info, var_info, meta_data)
}


# There are some restrictions on the three data structures.
# 1. The `expr_mat` should be a matrix with samples as columns and variables as rows.
# 2. The `sample_info` should be a tibble::tibble with a column named "sample".
# 3. The `var_info` should be a tibble::tibble with a column named "variable".
# 4. `colnames(expr_mat)` should be identical to `sample_info$sample`,
#    order doesn't matter.
# 5. `rownames(expr_mat)` should be identical to `var_info$variable`,
#    order doesn't matter.
new_experiment <- function(expr_mat, sample_info, var_info, meta_data) {
  stopifnot(is.matrix(expr_mat))
  stopifnot(tibble::is_tibble(sample_info))
  stopifnot(tibble::is_tibble(var_info))
  experiment <- list(
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    meta_data = meta_data
  )
  class(experiment) <- "glyexp_experiment"
  return(experiment)
}


#' Check if an Object is an Experiment
#'
#' This function checks if an object is an experiment,
#' i.e. if it inherits from the class `glyexp_experiment`.
#'
#' @param x An object to check.
#' @return A logical value.
#' @export
is_experiment <- function(x) {
  return(inherits(x, "glyexp_experiment"))
}
