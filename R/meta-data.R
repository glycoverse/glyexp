#' Get the meta data of an experiment
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
#' @template deprecated-experiment
#' @export
get_meta_data <- function(exp, x = NULL) {
  .deprecate_experiment_api("get_meta_data()")
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
  .deprecate_experiment_api("get_exp_type()")
  checkmate::assert_class(exp, "glyexp_experiment")
  exp$meta_data$exp_type
}


#' @rdname get_meta_data
#' @export
get_glycan_type <- function(exp) {
  .deprecate_experiment_api("get_glycan_type()")
  checkmate::assert_class(exp, "glyexp_experiment")
  exp$meta_data$glycan_type
}


#' Set the meta data of an experiment
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
#' @template deprecated-experiment
#' @export
set_meta_data <- function(exp, x, value) {
  .deprecate_experiment_api("set_meta_data()")
  .set_meta_data(exp, x, value)
}


#' Set legacy experiment metadata without a deprecation warning
#'
#' @inheritParams set_meta_data
#' @returns A modified `glyexp_experiment` object.
#' @noRd
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
  .deprecate_experiment_api("set_exp_type()")
  .set_meta_data(exp, "exp_type", value)
}


#' @rdname set_meta_data
#' @export
set_glycan_type <- function(exp, value) {
  .deprecate_experiment_api("set_glycan_type()")
  .set_meta_data(exp, "glycan_type", value)
}
