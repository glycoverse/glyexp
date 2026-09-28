#' Convert an experiment to a tibble
#'
#' @description
#' Convert an experiment object to a tibble of "tidy" format.
#' That is, each row is a unique combination of "sample" and "variable",
#' with the observation (the abundance) in the "value" column.
#' Additional columns in the sample and variable information are included.
#' This format is also known as the "long" format.
#'
#' Usually you don't want all columns in the sample information or variable information
#' tibbles to be included in the output tibble,
#' as this will make the output tibble very "wide".
#' You can specify which columns to include in the output tibble
#' by passing the column names to the `sample_cols` and `var_cols` arguments.
#' <[`data-masking`][rlang::args_data_masking]> syntax is used here.
#' By default, all columns are included.
#'
#' @param x An [experiment()].
#' @param sample_cols <[`data-masking`][rlang::args_data_masking]> Columns to include from the sample information tibble.
#' @param var_cols <[`data-masking`][rlang::args_data_masking]> Columns to include from the variable information tibble.
#' @param ... Ignored.
#'
#' @return A tibble.
#'
#' @template deprecated-experiment
#' @importFrom tibble as_tibble
#' @export
as_tibble.glyexp_experiment <- function(
  x,
  sample_cols = tidyselect::everything(),
  var_cols = tidyselect::everything(),
  ...
) {
  .deprecate_experiment_api("as_tibble.glyexp_experiment()")
  stopifnot(.is_experiment(x))
  # Convert the expression matrix to a long format tibble
  tb <- x$expr_mat |>
    as.data.frame() |>
    tibble::rownames_to_column("variable") |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      -all_of("variable"),
      names_to = "sample",
      values_to = "value"
    )
  # Join with sample_info and var_info
  sub_sample_info <- select_data(
    x$sample_info,
    "sample_info",
    "sample",
    {{ sample_cols }}
  )
  sub_var_info <- select_data(
    x$var_info,
    "var_info",
    "variable",
    {{ var_cols }}
  )
  tb <- tb |>
    dplyr::left_join(sub_sample_info, by = "sample") |>
    dplyr::left_join(sub_var_info, by = "variable")
  # Reorder columns: sample, sample fields, variable, variable fields, value
  sample_fields <- setdiff(colnames(sub_sample_info), "sample")
  var_fields <- setdiff(colnames(sub_var_info), "variable")
  cols <- c("sample", sample_fields, "variable", var_fields, "value")
  tb <- dplyr::select(tb, all_of(cols))
  tb
}

#' Convert a glycomics container to a long-format tibble
#'
#' @description
#' Each row represents one variable and sample pair, including missing abundance
#' values. Variables follow row order, with samples varying fastest.
#' Sample and variable identifiers come from the dimension names; missing names
#' are represented by `NA_character_`. Annotations are matched by position.
#'
#' @param x A [GlycomicSE()] or [GlycoproteomicSE()] object.
#' @param sample_cols,var_cols Tidy-select expressions selecting annotations from
#'   `colData(x)` and `rowData(x)`, respectively. Defaults to all columns.
#'   Use `NULL` to omit annotations.
#' @param ... Ignored.
#' @return A tibble with `sample`, sample annotations, `variable`, variable
#'   annotations, and `value` columns. Conflicting annotation names receive unique
#'   suffixes; `sample`, `variable`, and `value` retain their names.
#' @name as_tibble.GlycomicSE
#' @examples
#' tibble::as_tibble(real_experiment2, sample_cols = NULL, var_cols = NULL)
#' @export
as_tibble.GlycomicSE <- function(
  x,
  sample_cols = tidyselect::everything(),
  var_cols = tidyselect::everything(),
  ...
) {
  .as_tibble_glyco_se(x, {{ sample_cols }}, {{ var_cols }})
}

#' @rdname as_tibble.GlycomicSE
#' @export
as_tibble.GlycoproteomicSE <- function(
  x,
  sample_cols = tidyselect::everything(),
  var_cols = tidyselect::everything(),
  ...
) {
  .as_tibble_glyco_se(x, {{ sample_cols }}, {{ var_cols }})
}

.as_tibble_glyco_se <- function(x, sample_cols, var_cols) {
  samples <- tibble::as_tibble(SummarizedExperiment::colData(x)) |>
    dplyr::select({{ sample_cols }})
  variables <- tibble::as_tibble(SummarizedExperiment::rowData(x)) |>
    dplyr::select({{ var_cols }})
  sample_index <- rep(seq_len(ncol(x)), times = nrow(x))
  variable_index <- rep(seq_len(nrow(x)), each = ncol(x))
  sample_ids <- colnames(x)
  variable_ids <- rownames(x)
  if (is.null(sample_ids)) {
    sample_ids <- rep(NA_character_, ncol(x))
  }
  if (is.null(variable_ids)) {
    variable_ids <- rep(NA_character_, nrow(x))
  }

  annotation_names <- make.unique(c(
    "sample",
    "variable",
    "value",
    names(samples),
    names(variables)
  ))[-seq_len(3)]
  names(samples) <- utils::head(annotation_names, ncol(samples))
  names(variables) <- utils::tail(annotation_names, ncol(variables))
  tibble::tibble(
    sample = sample_ids[sample_index],
    !!!samples[sample_index, , drop = FALSE],
    variable = variable_ids[variable_index],
    !!!variables[variable_index, , drop = FALSE],
    value = as.vector(t(as.matrix(SummarizedExperiment::assay(x))))
  )
}
