#' Convert experiment to SummarizedExperiment
#'
#' @description
#' Convert an [experiment()] object to a `SummarizedExperiment` object.
#' This function maps the experiment structure to SummarizedExperiment format:
#' - `expr_mat` becomes the main assay
#' - `sample_info` becomes `colData`
#' - `var_info` becomes `rowData`
#' - `meta_data` becomes `metadata`
#'
#' @param exp An [experiment()] object to convert.
#' @param assay_name Character string specifying the name for the assay.
#'   Default is "abundance".
#'
#' @return A `SummarizedExperiment` object.
#'
#' @examples
#' # Convert toy experiment to SummarizedExperiment
#' se <- as_se(toy_experiment)
#' se
#'
#' @template deprecated-experiment
#' @export
as_se <- function(exp, assay_name = "abundance") {
  .deprecate_experiment_api("as_se()")
  .as_se(exp, assay_name)
}


#' Convert a legacy experiment without a deprecation warning
#'
#' @inheritParams as_se
#' @returns A `SummarizedExperiment` object.
#' @noRd
.as_se <- function(exp, assay_name = "abundance") {
  .require_se()

  # Check input
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(assay_name)

  # Extract components from experiment
  expr_mat <- exp$expr_mat
  sample_info <- exp$sample_info
  var_info <- exp$var_info
  meta_data <- exp$meta_data

  # Prepare colData (sample info)
  # Convert to data.frame and set rownames, remove the "sample" column
  coldata <- as.data.frame(sample_info)
  rownames(coldata) <- coldata$sample
  coldata$sample <- NULL

  # Prepare rowData (variable info)
  # Convert to data.frame and set rownames, remove the "variable" column
  rowdata <- as.data.frame(var_info)
  rownames(rowdata) <- rowdata$variable
  rowdata$variable <- NULL

  # Create SummarizedExperiment
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(expr_mat),
    colData = coldata,
    rowData = rowdata,
    metadata = meta_data
  )

  # Set assay name
  names(SummarizedExperiment::assays(se)) <- assay_name

  return(se)
}


#' Convert SummarizedExperiment to experiment
#'
#' @description
#' Convert a `SummarizedExperiment` object to an [experiment()] object.
#' This function maps the SummarizedExperiment structure to experiment format:
#' - The main assay becomes `expr_mat`
#' - `colData` becomes `sample_info`
#' - `rowData` becomes `var_info`
#' - `metadata` becomes `meta_data`
#'
#' @param se A `SummarizedExperiment` object to convert.
#' @param assay_name Character string specifying which assay to use.
#'   If NULL (default), uses the first assay.
#' @param exp_type Character string specifying experiment type.
#'   Must be either "glycomics", "glycoproteomics", "traitomics",
#'   "traitproteomics", or "others".
#'   If not supplied, will try to extract from metadata.
#'   If unavailable there, an error is issued.
#' @param glycan_type Character string specifying glycan type.
#'   Must be one of the valid `glycan_type` values accepted by [experiment()].
#'   If not supplied, will try to extract from metadata.
#'   If unavailable there, an error is issued unless `exp_type` is "others",
#'   where `NULL` is allowed.
#'
#' @return An [experiment()] object.
#'
#' @examples
#' # Convert SummarizedExperiment back to experiment
#' se <- as_se(toy_experiment)
#' exp_back <- from_se(se, exp_type = "glycomics", glycan_type = "N")
#' exp_back
#'
#' @template deprecated-experiment
#' @export
from_se <- function(
  se,
  assay_name = NULL,
  exp_type = NULL,
  glycan_type = NULL
) {
  .deprecate_experiment_api("from_se()")
  .require_se()

  exp_type_missing <- missing(exp_type)
  glycan_type_missing <- missing(glycan_type)

  # Check input
  checkmate::assert_class(se, "SummarizedExperiment")
  checkmate::assert_string(assay_name, null.ok = TRUE)
  checkmate::assert_choice(
    exp_type,
    c(
      "glycomics",
      "glycoproteomics",
      "traitomics",
      "traitproteomics",
      "others"
    ),
    null.ok = TRUE
  )
  checkmate::assert_choice(
    glycan_type,
    .valid_glycan_types(),
    null.ok = TRUE
  )

  # Get metadata
  meta_data <- S4Vectors::metadata(se)

  # Determine exp_type and glycan_type
  if (isTRUE(exp_type_missing)) {
    exp_type <- meta_data$exp_type
  }
  if (is.null(exp_type)) {
    cli::cli_abort(
      c(
        "{.arg exp_type} is not available in the {.cls SummarizedExperiment} metadata.",
        "i" = "Provide it through the `exp_type` argument."
      ),
      call = rlang::caller_env()
    )
  }
  if (isTRUE(glycan_type_missing)) {
    glycan_type <- meta_data$glycan_type
  }
  if (
    isTRUE(glycan_type_missing) &&
      is.null(glycan_type) &&
      exp_type != "others"
  ) {
    cli::cli_abort(
      c(
        "{.arg glycan_type} is not available in the {.cls SummarizedExperiment} metadata.",
        "i" = "Provide it through the `glycan_type` argument."
      ),
      call = rlang::caller_env()
    )
  }

  # Validate required parameters
  checkmate::assert_choice(
    exp_type,
    c("glycomics", "glycoproteomics", "traitomics", "traitproteomics", "others")
  )
  checkmate::assert_choice(
    glycan_type,
    .valid_glycan_types(),
    null.ok = exp_type == "others"
  )

  # Extract expression matrix
  if (is.null(assay_name)) {
    expr_mat <- SummarizedExperiment::assay(se, 1)
  } else {
    expr_mat <- SummarizedExperiment::assay(se, assay_name)
  }

  # Extract sample info
  coldata <- SummarizedExperiment::colData(se)
  sample_info <- tibble::as_tibble(coldata, rownames = "sample")

  # Extract variable info
  rowdata <- SummarizedExperiment::rowData(se)
  var_info <- tibble::as_tibble(rowdata, rownames = "variable")

  # Update metadata with required fields
  meta_data$exp_type <- exp_type
  meta_data$glycan_type <- glycan_type

  # Create experiment object using the constructor
  exp <- .experiment(
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = exp_type,
    glycan_type = glycan_type,
    check_col_types = FALSE
  )
  exp$meta_data <- meta_data
  exp
}

.require_se <- function() {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg SummarizedExperiment} must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("S4Vectors", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg S4Vectors} must be installed to use this function.",
      call. = FALSE
    )
  }
}
