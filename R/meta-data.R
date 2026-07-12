#' Get the meta data of an experiment
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These metadata helpers were deprecated with `experiment()`. Use
#' `S4Vectors::metadata()` on a `SummarizedExperiment` instead.
#'
#' Meta data is some descriptions about the experiment,
#' like the experiment type ("glycomics" or "glycoproteomics"),
#' or the glycan type ("N", "O", "O-GalNAc", "O-Man", "O-Fuc",
#' "O-GlcNAc", "O-Glc", "HMO", "GSL", "GAG", or "GPI").
#'
#' @param exp An [experiment()].
#' @param x A string, the name of the meta data field.
#'   If `NULL` (default), a list of all meta data fields will be returned.
#'
#' @returns The value of the meta data field. If the field does not exist,
#' `NULL` will be returned.
#' @examples
#' get_meta_data(real_experiment)
#' get_exp_type(real_experiment)
#' get_glycan_type(real_experiment)
#' @keywords internal
#' @export
get_meta_data <- function(exp, x = NULL) {
  .deprecate_experiment("get_meta_data()", "S4Vectors::metadata()")
  checkmate::assert_class(exp, "glyexp_experiment")
  if (is.null(x)) {
    return(exp$meta_data)
  } else {
    return(exp$meta_data[[x]])
  }
}


#' @rdname get_meta_data
#' @export
get_exp_type <- function(exp) {
  .deprecate_experiment("get_exp_type()", "S4Vectors::metadata()")
  exp$meta_data$exp_type
}


#' @rdname get_meta_data
#' @export
get_glycan_type <- function(exp) {
  .deprecate_experiment("get_glycan_type()", "S4Vectors::metadata()")
  exp$meta_data$glycan_type
}


#' Set the meta data of an experiment
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These metadata setters were deprecated with `experiment()`. Modify
#' `S4Vectors::metadata()` on a `SummarizedExperiment` instead.
#'
#' Set meta data values for the experiment,
#' like the experiment type ("glycomics" or "glycoproteomics"),
#' or the glycan type ("N", "O", "O-GalNAc", "O-Man", "O-Fuc",
#' "O-GlcNAc", "O-Glc", "HMO", "GSL", "GAG", or "GPI").
#'
#' @param exp An [experiment()].
#' @param x A string,
#' the name of the meta data field.
#' @param value The value to set for the meta data field.
#'
#' @returns The modified [experiment()] object.
#' @keywords internal
#' @export
set_meta_data <- function(exp, x, value) {
  .deprecate_experiment("set_meta_data()", "S4Vectors::metadata()")
  .set_meta_data(exp, x, value)
}

.set_meta_data <- function(exp, x, value) {
  checkmate::assert_class(exp, "glyexp_experiment")
  new_meta_data <- exp$meta_data
  new_meta_data[[x]] <- value
  .check_meta_data(new_meta_data)
  # Check if var_info has required columns when exp_type is changed
  if (x == "exp_type") {
    .check_required_cols(exp$var_info, value)
  }
  exp$meta_data[[x]] <- value
  exp
}


#' @rdname set_meta_data
#' @export
set_exp_type <- function(exp, value) {
  .deprecate_experiment("set_exp_type()", "S4Vectors::metadata()")
  .set_meta_data(exp, "exp_type", value)
}


#' @rdname set_meta_data
#' @export
set_glycan_type <- function(exp, value) {
  .deprecate_experiment("set_glycan_type()", "S4Vectors::metadata()")
  .set_meta_data(exp, "glycan_type", value)
}
