extract_missing_column <- function(error_msg) {
  patterns <- c(
    "object '([^']+)' not found",
    "Column `([^`]+)` doesn't exist",
    "Column `([^`]+)` not found in `\\.data`",
    "Element `([^`]+)` doesn't exist"
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

#' Abort when a virtual identifier has no corresponding dimension names
#'
#' @param id_column Either `".sample"` or `".variable"`.
#'
#' @return This function does not return.
#' @noRd
abort_missing_tidy_identifier <- function(id_column) {
  dimnames_accessor <- if (id_column == ".sample") {
    "colnames(exp)"
  } else {
    "rownames(exp)"
  }
  mutate_verb <- if (id_column == ".sample") {
    "mutate_col"
  } else {
    "mutate_row"
  }

  cli::cli_abort(
    c(
      "Cannot use {.field {id_column}} because `{dimnames_accessor}` does not exist.",
      "i" = "Create it with `{mutate_verb}(exp, {id_column} = ...)`."
    ),
    call = NULL
  )
}

#' Abort after a tidy verb refers to a missing column
#'
#' @param missing_col The missing column name.
#' @param data_name The user-facing metadata name.
#' @param available_cols Available metadata columns.
#' @param id_column The physical or virtual identifier column.
#'
#' @return This function does not return.
#' @noRd
abort_missing_tidy_column <- function(
  missing_col,
  data_name,
  available_cols,
  id_column
) {
  if (
    identical(missing_col, id_column) &&
      !id_column %in% available_cols &&
      id_column %in% c(".sample", ".variable")
  ) {
    abort_missing_tidy_identifier(id_column)
  }

  abort_missing_column(missing_col, data_name, available_cols)
}

#' Check whether an object is supported by the tidy manipulation verbs
#'
#' @param exp An object to check.
#'
#' @return A logical scalar.
#' @noRd
is_tidy_container <- function(exp) {
  .is_experiment(exp) || methods::is(exp, "SummarizedExperiment")
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

#' Get the internal row-position column used by tidy manipulation verbs
#'
#' @return A reserved column name used to preserve exact SE dimension
#'   positions, including when dimension names are duplicated.
#' @noRd
tidy_position_column <- function() {
  "..glyexp_position.."
}

#' Abort when a tidy operation creates a reserved column
#'
#' @param column A reserved column name.
#' @param data_name The user-facing metadata name.
#'
#' @return This function does not return.
#' @noRd
abort_reserved_tidy_column <- function(column, data_name) {
  cli::cli_abort(
    c(
      "Column {.field {column}} in `{data_name}` is reserved for dimension names.",
      "i" = "Choose a different metadata column name."
    ),
    call = NULL
  )
}

#' Check that a tidy operation did not create reserved columns
#'
#' @param data A data frame to check.
#' @param reserved_columns Reserved column names.
#' @param data_name The user-facing metadata name.
#'
#' @return `data`, invisibly.
#' @noRd
check_reserved_tidy_columns <- function(data, reserved_columns, data_name) {
  reserved_column <- intersect(reserved_columns, colnames(data))[1]
  if (!is.na(reserved_column)) {
    abort_reserved_tidy_column(reserved_column, data_name)
  }
  invisible(data)
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
  if (.is_experiment(exp)) {
    return(exp[[info_field]])
  }

  data <- if (info_field == "sample_info") {
    SummarizedExperiment::colData(exp)
  } else {
    SummarizedExperiment::rowData(exp)
  }
  data <- tibble::as_tibble(data)

  position_column <- tidy_position_column()
  data_name <- if (info_field == "sample_info") {
    "colData(exp)"
  } else {
    "rowData(exp)"
  }
  reserved_column <- intersect(
    c(id_column, position_column),
    colnames(data)
  )[1]
  if (!is.na(reserved_column)) {
    abort_reserved_tidy_column(reserved_column, data_name)
  }

  data <- dplyr::mutate(
    data,
    "{position_column}" := seq_len(nrow(data)),
    .before = 1
  )

  ids <- if (info_field == "sample_info") colnames(exp) else rownames(exp)
  if (!is.null(ids)) {
    data <- dplyr::mutate(data, "{id_column}" := ids, .before = 1)
  }

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
  position_column <- tidy_position_column()

  if (isTRUE(subset)) {
    indices <- new_data[[position_column]]
    exp <- if (info_field == "sample_info") {
      exp[, indices, drop = FALSE]
    } else {
      exp[indices, , drop = FALSE]
    }
  }

  stored_data <- dplyr::select(
    new_data,
    -dplyr::any_of(c(id_column, position_column))
  )
  new_ids <- new_data[[id_column]]
  stored_data <- S4Vectors::DataFrame(stored_data, row.names = new_ids)

  if (info_field == "sample_info") {
    if (!is.null(new_ids)) {
      colnames(exp) <- new_ids
    }
    SummarizedExperiment::colData(exp) <- stored_data
  } else {
    if (!is.null(new_ids)) {
      rownames(exp) <- new_ids
    }
    SummarizedExperiment::rowData(exp) <- stored_data
  }

  exp
}
