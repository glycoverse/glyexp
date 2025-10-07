#' Create a new experiment
#'
#' The data container of a glycoproteomics or glycomics experiment.
#' Expression matrix, sample information, and variable information
#' are required then will be managed by the experiment object.
#' It acts as the data core of the `glycoverse` ecosystem.
#'
#' @details
#' # Requirements of the input data
#'
#' **Expression matrix:**
#'
#' - Must be a numeric matrix with **variables as rows** and **samples as columns**.
#' - The **column names** must correspond to sample IDs.
#' - The **row names** must correspond to variable IDs.
#'
#' **Sample information (`sample_info`):**
#'
#' - Must be a tibble with a column named "sample" (sample ID).
#' - Each value in "sample" must be unique.
#' - The set of "sample" values must match the column names of the expression matrix (order does not matter).
#'
#' **Variable information (`var_info`):**
#'
#' - Must be a tibble with a column named "variable" (variable ID).
#' - Each value in "variable" must be unique.
#' - The set of "variable" values must match the row names of the expression matrix (order does not matter).
#'
#' The function will automatically reorder the expression matrix
#' to match the order of "sample" and "variable" in the info tables.
#'
#' # Column requirements
#'
#' Some columns are required compulsorily in the variable information tibble for a valid experiment.
#' It depends on the experiment type.
#'
#' - For "glycomics": `glycan_composition`.
#' - For "glycoproteomics": `protein`, `protein_site`, `glycan_composition`.
#' - For "traitomics": no required columns.
#' - For "traitproteomics": `protein`, `protein_site`.
#' - For "others": no required columns.
#'
#' See the "Column conventions" section for detailed description of these columns.
#'
#' The last two types of experiments are created by the `glydet` package.
#' Normally you don't need to manually create them.
#'
#' # Column conventions
#'
#' `glycoverse` has some conserved column names for `sample_info` and `var_info` to make everything work seamlessly.
#' It's not mandatory, but following these conventions will make your life easier.
#'
#' **sample_info:**
#'
#' - `group`: factor, treatment/condition/grouping, used by many `glystats` and `glyvis` functions.
#' - `batch`: factor, batch information, used by `glyclean::correct_batch_effect()`.
#' - `bio_rep`: factor, biological replicate, may be used in the future.
#'
#' **var_info:**
#'
#' - `protein`: character, protein Uniprot accession.
#' - `protein_site`: integer, glycosylation site position on protein.
#' - `gene`: character, gene symbol.
#' - `peptide`: character, peptide sequence.
#' - `peptide_site`: integer, glycosylation site position on peptide.
#' - `glycan_composition`: `glyrepr::glycan_composition()`, glycan composition.
#' - `glycan_structure`: `glyrepr::glycan_structure()`, glycan structure.
#'
#' # Meta data
#'
#' Other meta data can be added to the `meta_data` attribute.
#' `meta_data` is a list of additional information about the experiment.
#' Two meta data fields are required:
#'
#' - `exp_type`: "glycomics", "glycoproteomics", "traitomics", "traitproteomics", or "others"
#' - `glycan_type`: "N" or "O" (can be NULL if `exp_type` is "others")
#'
#' Other meta data will be added by other `glycoverse` packages for their own purposes.
#'
#' # Index columns
#'
#' The **index columns** are the backbone that keep your data synchronized:
#'
#' - The "sample" column in `sample_info` must match the column names of `expr_mat`.
#' - The "variable" column in `var_info` must match the row names of `expr_mat`.
#'
#' These columns act as unique identifiers,
#' ensuring that your expression matrix, sample information, and variable information always stay in sync, 
#' no matter how you filter, arrange, or subset your data.
#'
#' @param expr_mat An expression matrix with samples as columns and variables as rows.
#' @param sample_info A tibble with a column named "sample", and other
#'   columns other useful information about samples,
#'   e.g. group, batch, sex, age, etc.
#' @param var_info A tibble with a column named "variable", and other
#'   columns other useful information about variables,
#'   e.g. protein name, peptide, glycan composition, etc.
#' @param exp_type The type of the experiment,
#'   "glycomics", "glycoproteomics", "traitomics", "traitproteomics", or "others".
#'   Default to "others".
#' @param glycan_type The type of glycan, "N" or "O". Can be NULL if `exp_type` is "others".
#' @param coerce_col_types If common column types are coerced. Default to TRUE.
#'   If TRUE, all columns in the "Column conventions" section will be coerced to the expected types.
#'   Skipped for "others" type even if TRUE.
#' @param check_col_types If column type conventions are checked. Default to TRUE.
#'   Type checking is performed after column coercion (if `coerce_col_types` is TRUE).
#'   Skipped for "others" type even if TRUE.
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
#'   exp_type = "others",
#'   glycan_type = "N"
#' )
#'
#' @export
experiment <- function(
  expr_mat,
  sample_info,
  var_info,
  exp_type = "others",
  glycan_type = NULL,
  coerce_col_types = TRUE,
  check_col_types = TRUE,
  ...
) {
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
  checkmate::assert_choice(exp_type, c("glycomics", "glycoproteomics", "traitomics", "traitproteomics", "others"))
  checkmate::assert_choice(glycan_type, c("N", "O"), null.ok = TRUE)
  if (exp_type != "others" && is.null(glycan_type)) {
    cli::cli_abort("{.arg glycan_type} must be provided if {.arg exp_type} is not {.val others}.")
  }

  # Check if "sample" and "variable" columns are present in sample_info and var_info
  .check_index_cols(expr_mat, sample_info, var_info)

  # Check if samples and variables are unique
  .check_index_uniqueness(sample_info, var_info)

  # Check if samples and variables are consistent between expr_mat, sample_info, and var_info
  .check_expr_mat_info_consistency(expr_mat, sample_info, var_info)

  # Check if all required columns are present in var_info
  .check_required_cols(var_info, exp_type)

  # Normalize and check the column type conventions
  if (check_col_types && exp_type != "others") {
    # If column coercion is requested, do it first
    coerced_info <- .coerce_col_types(var_info, sample_info)
    var_info <- coerced_info$var_info
    sample_info <- coerced_info$sample_info
    # Check the column type conventions
    .check_col_types(var_info, sample_info)
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

# Check if "sample" and "variable" columns are present in sample_info and var_info
.check_index_cols <- function(expr_mat, sample_info, var_info) {
  if (!"sample" %in% colnames(sample_info)) {
    cli::cli_abort("`sample_info` must have a 'sample' column", call = rlang::caller_env())
  }
  if (!"variable" %in% colnames(var_info)) {
    cli::cli_abort("`var_info` must have a 'variable' column", call = rlang::caller_env())
  }
}

# Check if samples and variables are consistent between expr_mat, sample_info, and var_info
.check_expr_mat_info_consistency <- function(expr_mat, sample_info, var_info) {
  sample_check <- .check_info_consistency(
    colnames(expr_mat), sample_info$sample, 
    "Samples", "sample_info"
  )
  var_check <- .check_info_consistency(
    rownames(expr_mat), var_info$variable, 
    "Variables", "var_info"
  )

  # Stop if samples or variables are not consistent
  if (!sample_check$consistent || !var_check$consistent) {
    err_msg <- stringr::str_c(sample_check$error_msg, var_check$error_msg, sep = " ")
    cli::cli_abort(c(
      "Samples or variables must be consistent between `expr_mat`, `sample_info`, and `var_info`.",
      "x" = err_msg
    ), call = rlang::caller_env())
  }
}

# Check consistency between expression matrix and info tables
.check_info_consistency <- function(expr_names, info_names, expr_label, info_label) {
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

# Check if samples and variables are unique
.check_index_uniqueness <- function(sample_info, var_info) {
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
    ), call = rlang::caller_env())
  }
}

# Check if all required columns are present in var_info
.check_required_cols <- function(var_info, exp_type) {
  required_cols <- switch(exp_type,
    "glycomics" = c("glycan_composition"),
    "glycoproteomics" = c("protein", "protein_site", "glycan_composition"),
    "traitomics" = c(),
    "traitproteomics" = c("protein", "protein_site"),
    "others" = c()
  )
  missing_cols <- setdiff(required_cols, colnames(var_info))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "All required columns must be present in `var_info`.",
      "i" = "Required columns: {.field {required_cols}}.",
      "x" = "Missing columns: {.field {missing_cols}}."
    ), call = rlang::caller_env())
  }
}

# Coerce common column types to improve user experience
.coerce_col_types <- function(var_info, sample_info) {
  is_whole_number <- function(x, tol = .Machine$double.eps^0.5) {
    !is.na(x) & abs(x - round(x)) < tol
  }

  coerce_factor_col <- function(tbl, col) {
    if (col %in% colnames(tbl) && !inherits(tbl[[col]], "factor")) {
      vec <- tbl[[col]]
      if (is.atomic(vec)) {
        tbl[[col]] <- factor(vec)
        cli::cli_inform("Column {.field {col}} converted to {.cls factor}.")
      }
    }
    tbl
  }

  coerce_integer_col <- function(tbl, col) {
    if (col %in% colnames(tbl) && !inherits(tbl[[col]], "integer")) {
      vec <- tbl[[col]]
      if (is.numeric(vec) && !inherits(vec, "integer")) {
        non_na <- vec[!is.na(vec)]
        if (length(non_na) == 0 || all(is_whole_number(non_na))) {
          tbl[[col]] <- as.integer(round(vec))
          cli::cli_inform("Column {.field {col}} converted to {.cls integer}.")
        } else {
          cli::cli_alert_warning("Column {.field {col}} contains non-integer numeric values; kept as {.cls {class(vec)}}.")
        }
      }
    }
    tbl
  }

  coerce_character_col <- function(tbl, col) {
    if (col %in% colnames(tbl) && !inherits(tbl[[col]], "character")) {
      vec <- tbl[[col]]
      if (is.atomic(vec)) {
        tbl[[col]] <- as.character(vec)
        cli::cli_inform("Column {.field {col}} converted to {.cls character}.")
      }
    }
    tbl
  }

  sample_info <- coerce_factor_col(sample_info, "group")
  sample_info <- coerce_factor_col(sample_info, "batch")
  sample_info <- coerce_factor_col(sample_info, "bio_rep")

  var_info <- coerce_character_col(var_info, "protein")
  var_info <- coerce_integer_col(var_info, "protein_site")
  var_info <- coerce_character_col(var_info, "gene")
  var_info <- coerce_character_col(var_info, "peptide")
  var_info <- coerce_integer_col(var_info, "peptide_site")

  list(var_info = var_info, sample_info = sample_info)
}

# Check the column type conventions
.check_col_types <- function(var_info, sample_info) {
  sample_col_types <- c(
    "group" = "factor",
    "batch" = "factor",
    "bio_rep" = "factor"
  )
  var_col_types <- c(
    "protein" = "character",
    "protein_site" = "integer",
    "gene" = "character",
    "peptide" = "character",
    "peptide_site" = "integer",
    "glycan_composition" = "glyrepr_composition",
    "glycan_structure" = "glyrepr_structure"
  )

  .check_fn <- function(col_types, info_tbl, info_label) {
    violated <- FALSE
    for (i in seq_along(col_types)) {
      col_name <- names(col_types)[[i]]
      col_type <- col_types[[i]]
      if (col_name %in% colnames(info_tbl)) {
        if (!inherits(info_tbl[[col_name]], col_type)) {
          cli::cli_alert_warning("Column {.field {col_name}} should be {.cls {col_type}} instead of {.cls {class(info_tbl[[col_name]])}}.")
          violated <- TRUE
        }
      }
    }
    if (violated) {
      cli::cli_alert_info("Some column type conventions are violated for {.field {info_label}}.")
      cli::cli_alert_info("Consider correcting them and create a new experiment.")
    }
  }

  .check_fn(sample_col_types, sample_info, "sample_info")
  .check_fn(var_col_types, var_info, "var_info")
}
