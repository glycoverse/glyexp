#' Standardize variable IDs in an experiment
#'
#' @description
#' Converts meaningless variable IDs (like "GP1", "V1") into meaningful,
#' human-readable IDs based on the experiment type and available columns.
#'
#' @param exp An [experiment()].
#' @param format A format string specifying how to construct variable IDs.
#'   Use `{column_name}` to insert values from `var_info` columns.
#'   If `NULL` (default), a sensible format is chosen based on `exp_type`.
#' @param unique_suffix A string pattern for making IDs unique when duplicates exist.
#'   Must contain `{N}` which will be replaced with the numeric suffix.
#'   Default is `"-{N}"`.
#'
#' @return The experiment with standardized variable IDs, invisibly.
#'
#' @examples
#' # See examples in the package documentation
#'
#' @export
standardize_variable <- function(exp, format = NULL, unique_suffix = "-{N}") {
  if (!is_experiment(exp)) {
    cli::cli_abort("{.arg exp} must be an experiment.")
  }

  # Validate unique_suffix contains {N}
  if (!stringr::str_detect(unique_suffix, "\\{N\\}")) {
    cli::cli_abort("{.arg unique_suffix} must contain {.val {N}} as a placeholder.")
  }

  exp_type <- exp$meta_data$exp_type
  var_info <- exp$var_info

  # Determine format if not provided
  if (is.null(format)) {
    format <- .get_default_format(exp_type, var_info)
  }

  # Generate new variable IDs using glue
  new_vars <- .glue_with_composition(var_info, format)

  # Ensure uniqueness by adding suffix if needed
  new_vars <- .ensure_unique(new_vars, unique_suffix)

  # Update var_info and expr_mat
  exp$var_info$variable <- new_vars
  rownames(exp$expr_mat) <- new_vars

  invisible(exp)
}

#' Format variable strings with special handling for glycan_composition
#' @keywords internal
.glue_with_composition <- function(var_info, format) {
  # Check if format contains glycan_composition and it's a list column
  if (stringr::str_detect(format, "\\{glycan_composition\\}")) {
    if ("glycan_composition" %in% colnames(var_info) && is.list(var_info$glycan_composition)) {
      # Convert glycan_composition list to character for this operation
      gc_char <- vapply(var_info$glycan_composition, as.character, FUN.VALUE = character(1))
      var_info$glycan_composition_char <- gc_char
      format <- stringr::str_replace_all(format, "\\{glycan_composition\\}", "{glycan_composition_char}")
    }
  }
  glue::glue_data(var_info, format)
}

#' Get default format string based on exp_type
#' @keywords internal
.get_default_format <- function(exp_type, var_info) {
  switch(exp_type,
    "glycomics" = {
      if (!"glycan_composition" %in% colnames(var_info)) {
        cli::cli_abort("glycan_composition column is required for glycomics experiments.")
      }
      "{glycan_composition}"
    },
    "glycoproteomics" = {
      if (!"glycan_composition" %in% colnames(var_info)) {
        cli::cli_abort("glycan_composition column is required for glycoproteomics experiments.")
      }
      if ("protein_site" %in% colnames(var_info) && !all(is.na(var_info$protein_site))) {
        "{protein}-{protein_site}-{glycan_composition}"
      } else {
        "{protein}-{glycan_composition}"
      }
    },
    "traitomics" = {
      if ("motif" %in% colnames(var_info) && !all(is.na(var_info$motif))) {
        "{motif}"
      } else if ("trait" %in% colnames(var_info)) {
        "{trait}"
      } else {
        cli::cli_abort("Either 'motif' or 'trait' column is required for traitomics experiments.")
      }
    },
    "traitproteomics" = {
      if (!"protein_site" %in% colnames(var_info)) {
        cli::cli_abort("protein_site column is required for traitproteomics experiments.")
      }
      if ("motif" %in% colnames(var_info) && !all(is.na(var_info$motif))) {
        "{protein}-{protein_site}-{motif}"
      } else if ("trait" %in% colnames(var_info)) {
        "{protein}-{protein_site}-{trait}"
      } else {
        cli::cli_abort("Either 'motif' or 'trait' column is required for traitproteomics experiments.")
      }
    },
    cli::cli_abort("exp_type '{exp_type}' is not supported.")
  )
}

#' Ensure variable IDs are unique by adding numeric suffixes
#' @keywords internal
.ensure_unique <- function(vars, unique_suffix) {
  if (length(vars) == 0) return(vars)

  if (length(unique(vars)) == length(vars)) {
    return(vars)
  }

  # Count occurrences of each value
  var_counts <- table(vars)

  # Track current count for each unique value
  current_counts <- stats::setNames(rep(0, length(var_counts)), names(var_counts))
  result <- character(length(vars))

  for (i in seq_along(vars)) {
    v <- vars[[i]]
    if (var_counts[[v]] > 1) {
      current_counts[[v]] <- current_counts[[v]] + 1
      suffix <- stringr::str_replace(unique_suffix, "\\{N\\}", as.character(current_counts[[v]]))
      result[[i]] <- paste0(v, suffix)
    } else {
      result[[i]] <- v
    }
  }

  result
}
