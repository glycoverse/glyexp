#' Merge two experiments
#'
#' @description
#' Merges two [experiment()] objects into a single object.
#'
#' **Expression matrix:**
#' Expression matrices are merged by matching variables.
#' For variables that are present in only one of the experiments,
#' the corresponding values in the expression matrix are set to `NA`.
#'
#' **Sample information:**
#' Column names and column types must be identical (order does not matter).
#' No overlap is allowed for sample names.
#'
#' **Variable information:**
#' Column names and column types must be identical (order does not matter).
#' Variable names are ignored.
#' The identity of variables is determined by all other columns in the variable information.
#' If row A in the first `var_info` is identical (order does not matter)
#' to row B in the second `var_info`,
#' they are considered the same variable.
#' Therefore, please make sure that each row has a unique combination of values (except for `variable`),
#' otherwise the function cannot determine which variable in `x` is identical to which variable in `y`.
#' To ensure uniqueness, you can use `glyclean::aggregate()`.
#'
#' **Metadata:**
#' Metadata is taken from the first experiment.
#' This is the only place where the order of `x` and `y` matters.
#'
#' @details
#' # Variable matching
#'
#' In the variable information tibble,
#' the `variable` column is just arbitrary unique identifier,
#' while the real identity of a variable is determined by all other columns.
#' For example, if the variable information tibble has the following columns:
#'
#' ```
#' variable glycan_composition protein protein_site
#' V1       Hex(2)HexNAc(1)    P1      2
#' V2       Hex(2)HexNAc(1)    P2      3
#' V3       Hex(2)HexNAc(2)    P2      3
#' V4       Hex(2)HexNAc(2)    P2      3
#' ```
#' We know that V1, V2, and V3 are all different variables,
#' but V3 and V4 are the same variable (they have the same glycan composition, protein, and protein site).
#'
#' During the merge, the function will use all columns in the variable information tibble
#' except for `variable` to match variables.
#' This means that if the combination of all other columns is not unique,
#' the function cannot determine the identity of the variables.
#'
#' To ensure uniqueness, you can use `glyclean::aggregate()`,
#' which merges the expression levels of variables with the same identity.
#'
#' After the merge, a new `variable` column is added to the variable information tibble,
#' and used as the rownames of the expression matrix.
#'
#' # Variable and sample orders
#'
#' The order of variables and samples in the merged experiment is deterministic.
#'
#' For samples, as no overlapping is allowed for sample names,
#' the new sample names are the union of the sample names in `x` and `y`,
#' orders reserved.
#'
#' For variables, the new variables are:
#' 1. First, all variables in `x`, with the same order as in `x`.
#' 2. Then, all variables in `y` while not in `x`, with the same order as in `y`.
#'
#' Note that for variables, we refer to the identity of variables,
#' not the variable names in the `variable` column.
#'
#' @param x,y [experiment()] objects to merge
#' @param ... Not used
#'
#' @return A new [experiment()] object
#' @export
merge.glyexp_experiment <- function(x, y, ...) {
  # Validate inputs
  checkmate::assert_class(x, "glyexp_experiment")
  checkmate::assert_class(y, "glyexp_experiment")
  
  # Check sample information compatibility
  .check_sample_info_compatibility(x$sample_info, y$sample_info)
  
  # Check variable information compatibility  
  .check_var_info_compatibility(x$var_info, y$var_info)
  
  # Check for overlapping samples
  overlapping_samples <- intersect(x$sample_info$sample, y$sample_info$sample)
  if (length(overlapping_samples) > 0) {
    cli::cli_abort('Overlapping samples: {.val {overlapping_samples}}')
  }
  
  # Check variable information uniqueness
  .check_var_info_uniqueness(x$var_info)
  .check_var_info_uniqueness(y$var_info)
  
  # Merge sample information
  merged_sample_info <- dplyr::bind_rows(x$sample_info, y$sample_info)
  
  # Identify and merge variable information
  var_merge_result <- .merge_variable_info(x$var_info, y$var_info)
  merged_var_info <- var_merge_result$merged_var_info
  var_mapping_x <- var_merge_result$var_mapping_x
  var_mapping_y <- var_merge_result$var_mapping_y
  
  # Merge expression matrices
  merged_expr_mat <- .merge_expression_matrices(
    x$expr_mat, y$expr_mat,
    var_mapping_x, var_mapping_y,
    merged_sample_info$sample, merged_var_info$variable
  )
  
  # Use metadata from first experiment
  merged_meta_data <- x$meta_data
  
  # Create new experiment
  experiment(
    merged_expr_mat, 
    merged_sample_info, 
    merged_var_info,
    merged_meta_data$exp_type,
    merged_meta_data$glycan_type
  )
}

# Helper function to check sample info compatibility
.check_sample_info_compatibility <- function(sample_info_1, sample_info_2) {
  # Check column names
  cols_1 <- colnames(sample_info_1)
  cols_2 <- colnames(sample_info_2)
  if (!setequal(cols_1, cols_2)) {
    cli::cli_abort("Column names in sample information do not match")
  }
  
  # Check column types
  for (col in cols_1) {
    if (!identical(class(sample_info_1[[col]]), class(sample_info_2[[col]]))) {
      cli::cli_abort("Column types in sample information do not match")
    }
  }
}

# Helper function to check variable info compatibility
.check_var_info_compatibility <- function(var_info_1, var_info_2) {
  # Check column names
  cols_1 <- colnames(var_info_1)
  cols_2 <- colnames(var_info_2)
  if (!setequal(cols_1, cols_2)) {
    cli::cli_abort("Column names in variable information do not match")
  }
  
  # Check column types
  for (col in cols_1) {
    if (!identical(class(var_info_1[[col]]), class(var_info_2[[col]]))) {
      cli::cli_abort("Column types in variable information do not match")
    }
  }
}

# Helper function to check variable info uniqueness
.check_var_info_uniqueness <- function(var_info) {
  # Exclude the 'variable' column for uniqueness check
  identity_cols <- setdiff(colnames(var_info), "variable")
  if (length(identity_cols) > 0) {
    identity_data <- var_info[identity_cols]
    if (anyDuplicated(identity_data)) {
      cli::cli_abort("Variable information is not unique")
    }
  }
}

# Helper function to merge variable information
.merge_variable_info <- function(var_info_1, var_info_2) {
  identity_cols <- setdiff(colnames(var_info_1), "variable")
  
  if (length(identity_cols) == 0) {
    # If there are no identity columns, all variables are considered unique.
    # Their identity is just their original position.
    merged_var_info <- dplyr::bind_rows(var_info_1, var_info_2) %>%
      dplyr::mutate(variable = paste0("V", dplyr::row_number()))
    
    var_mapping_x <- seq_len(nrow(var_info_1))
    var_mapping_y <- nrow(var_info_1) + seq_len(nrow(var_info_2))
  } else {
    identity_1 <- dplyr::select(var_info_1, -"variable")
    identity_2 <- dplyr::select(var_info_2, -"variable")
    
    # Get merged distinct variable definitions, preserving order of first appearance
    merged_identity <- dplyr::distinct(dplyr::bind_rows(identity_1, identity_2))
    
    # Add an index to the distinct set for joining
    merged_identity_with_idx <- merged_identity %>%
      dplyr::mutate(.merged_idx = dplyr::row_number())
      
    # Map original var_info rows to the new distinct indices
    map_x <- identity_1 %>%
      dplyr::mutate(.orig_idx = dplyr::row_number()) %>%
      dplyr::left_join(merged_identity_with_idx, by = identity_cols) %>%
      dplyr::arrange(.data$.orig_idx)
      
    map_y <- identity_2 %>%
      dplyr::mutate(.orig_idx = dplyr::row_number()) %>%
      dplyr::left_join(merged_identity_with_idx, by = identity_cols) %>%
      dplyr::arrange(.data$.orig_idx)
      
    var_mapping_x <- map_x$.merged_idx
    var_mapping_y <- map_y$.merged_idx
    
    # Create final merged var_info with a new 'variable' column
    merged_var_info <- merged_identity %>%
      dplyr::mutate(variable = paste0("V", dplyr::row_number()), .before = 1)
  }
  
  list(
    merged_var_info = merged_var_info,
    var_mapping_x = var_mapping_x,
    var_mapping_y = var_mapping_y
  )
}

# Helper function to merge expression matrices
.merge_expression_matrices <- function(expr_mat_1, expr_mat_2, var_mapping_x, var_mapping_y, 
                                    sample_names, variable_names) {
  n_samples <- length(sample_names)
  n_variables <- length(variable_names)
  n_samples_1 <- ncol(expr_mat_1)
  n_samples_2 <- ncol(expr_mat_2)
  
  # Create merged expression matrix
  merged_expr_mat <- matrix(NA, nrow = n_variables, ncol = n_samples)
  rownames(merged_expr_mat) <- variable_names
  colnames(merged_expr_mat) <- sample_names
  
  # Fill in values from first experiment
  for (i in seq_len(nrow(expr_mat_1))) {
    var_idx <- var_mapping_x[i]
    merged_expr_mat[var_idx, 1:n_samples_1] <- expr_mat_1[i, ]
  }
  
  # Fill in values from second experiment
  for (i in seq_len(nrow(expr_mat_2))) {
    var_idx <- var_mapping_y[i]
    merged_expr_mat[var_idx, (n_samples_1 + 1):n_samples] <- expr_mat_2[i, ]
  }
  
  merged_expr_mat
}
