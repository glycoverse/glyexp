extract_missing_column <- function(error_msg) {
  patterns <- c(
    "object '([^']+)' not found",
    "Column `([^`]+)` doesn't exist"
  )

  for (pattern in patterns) {
    match <- stringr::str_match(error_msg, pattern)
    if (!is.na(match[1, 2])) {
      return(match[1, 2])
    }
  }

  NA_character_
}

abort_missing_column <- function(missing_col, data_name, available_cols) {
  cli::cli_abort(
    c(
      "Column {.field {missing_col}} not found in `{data_name}`.",
      "i" = "Available columns: {.field {available_cols}}"
    ),
    call = NULL
  )
}

#' Check whether an object is supported by the tidy manipulation verbs
#'
#' @param exp An object to check.
#'
#' @return A logical scalar.
#' @noRd
is_tidy_container <- function(exp) {
  is_experiment(exp) || methods::is(exp, "SummarizedExperiment")
}

#' Get the identifier column used by a tidy manipulation verb
#'
#' @param exp An [experiment()] or `SummarizedExperiment` object.
#' @param id_column Either `"sample"` or `"variable"`.
#'
#' @return `id_column` for an [experiment()] object, or its dot-prefixed virtual
#'   equivalent for a `SummarizedExperiment` object.
#' @noRd
tidy_id_column <- function(exp, id_column) {
  if (methods::is(exp, "SummarizedExperiment")) {
    paste0(".", id_column)
  } else {
    id_column
  }
}

#' Extract metadata for a tidy manipulation verb
#'
#' For `SummarizedExperiment` objects, row or column names are exposed as a
#' transient `.variable` or `.sample` column.
#'
#' @param exp An [experiment()] or `SummarizedExperiment` object.
#' @param info_field Either `"sample_info"` or `"var_info"`.
#' @param id_column The physical or virtual identifier column.
#'
#' @return A tibble containing the requested metadata and identifier column.
#' @noRd
tidy_info_data <- function(exp, info_field, id_column) {
  if (is_experiment(exp)) {
    return(exp[[info_field]])
  }

  data <- if (info_field == "sample_info") {
    SummarizedExperiment::colData(exp)
  } else {
    SummarizedExperiment::rowData(exp)
  }
  data <- tibble::as_tibble(data)

  if (id_column %in% colnames(data)) {
    data_name <- if (info_field == "sample_info") {
      "colData(exp)"
    } else {
      "rowData(exp)"
    }
    cli::cli_abort(
      c(
        "Column {.field {id_column}} in `{data_name}` is reserved for dimension names.",
        "i" = "Rename the metadata column before using tidy manipulation verbs."
      ),
      call = NULL
    )
  }

  ids <- if (info_field == "sample_info") colnames(exp) else rownames(exp)
  if (is.null(ids)) {
    ids <- as.character(seq_len(nrow(data)))
  }
  data <- dplyr::mutate(data, "{id_column}" := ids, .before = 1)

  data
}

#' Update metadata and dimensions of a SummarizedExperiment
#'
#' @param exp A `SummarizedExperiment` object.
#' @param new_data A data frame containing updated metadata.
#' @param info_field Either `"sample_info"` or `"var_info"`.
#' @param id_column Either `".sample"` or `".variable"`.
#' @param subset Whether to subset and reorder the corresponding dimension.
#'
#' @return An updated object of the same class as `exp`.
#' @noRd
update_se_info <- function(
  exp,
  new_data,
  info_field,
  id_column,
  subset = FALSE
) {
  new_ids <- new_data[[id_column]]

  if (isTRUE(subset)) {
    original_ids <- tidy_info_data(exp, info_field, id_column)[[id_column]]
    indices <- match(new_ids, original_ids)
    exp <- if (info_field == "sample_info") {
      exp[, indices, drop = FALSE]
    } else {
      exp[indices, , drop = FALSE]
    }
  }

  stored_data <- dplyr::select(new_data, -dplyr::all_of(id_column))
  stored_data <- S4Vectors::DataFrame(stored_data, row.names = new_ids)

  if (info_field == "sample_info") {
    colnames(exp) <- new_ids
    SummarizedExperiment::colData(exp) <- stored_data
  } else {
    rownames(exp) <- new_ids
    SummarizedExperiment::rowData(exp) <- stored_data
  }

  exp
}
