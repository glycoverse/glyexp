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
#'   Default is "counts".
#'
#' @return A `SummarizedExperiment` object.
#'
#' @examples
#' # Convert toy experiment to SummarizedExperiment
#' se <- as_se(toy_experiment)
#' se
#'
#' @export
as_se <- function(exp, assay_name = "counts") {
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
#'   Must be either "glycomics", "glycoproteomics", or "others".
#'   If NULL, will try to extract from metadata, otherwise defaults to "glycomics".
#' @param glycan_type Character string specifying glycan type.
#'   Must be either "N" or "O".
#'   If NULL, will try to extract from metadata, otherwise defaults to "N".
#'
#' @return An [experiment()] object.
#'
#' @examples
#' # Convert SummarizedExperiment back to experiment
#' se <- as_se(toy_experiment)
#' exp_back <- from_se(se, exp_type = "glycomics", glycan_type = "N")
#' exp_back
#'
#' @export
from_se <- function(se, assay_name = NULL, exp_type = NULL, glycan_type = NULL) {
  .require_se()

  # Check input
  checkmate::assert_class(se, "SummarizedExperiment")
  checkmate::assert_string(assay_name, null.ok = TRUE)
  checkmate::assert_choice(exp_type, c("glycomics", "glycoproteomics", "others"), null.ok = TRUE)
  checkmate::assert_choice(glycan_type, c("N", "O"), null.ok = TRUE)
  
  # Get metadata
  meta_data <- S4Vectors::metadata(se)
  
  # Determine exp_type and glycan_type
  if (is.null(exp_type)) {
    exp_type <- meta_data$exp_type %||% "glycomics"
  }
  if (is.null(glycan_type)) {
    glycan_type <- meta_data$glycan_type %||% "N"
  }
  
  # Validate required parameters
  checkmate::assert_choice(exp_type, c("glycomics", "glycoproteomics", "others"))
  checkmate::assert_choice(glycan_type, c("N", "O"))
  
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
  experiment(
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = exp_type,
    glycan_type = glycan_type
  )
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