#' Get the meta data of an [experiment()]
#'
#' Meta data is some descriptions about the experiment,
#' like the experiment type ("glycomics" or "glycoproteomics"),
#' or the glycan type ("N" or "O").
#'
#' @param exp An [experiment()].
#' @param x A string, the name of the meta data field.
#'
#' @returns The value of the meta data field. If the field does not exist,
#' `NULL` will be returned.
#' @export
get_meta_data <- function(exp, x) {
  stopifnot(class(exp) == "glyexp_experiment")
  exp$meta_data[[x]]
}


#' @rdname get_meta_data
#' @export
get_exp_type <- function(exp) {
  stopifnot(class(exp) == "glyexp_experiment")
  exp$meta_data$exp_type
}


#' @rdname get_meta_data 
#' @export
get_glycan_type <- function(exp) {
  stopifnot(class(exp) == "glyexp_experiment")
  exp$meta_data$glycan_type
}


#' Set the meta data of an [experiment()]
#'
#' Set meta data values for the experiment,
#' like the experiment type ("glycomics" or "glycoproteomics"),
#' or the glycan type ("N" or "O").
#'
#' @param exp An [experiment()].
#' @param x A string,
#' the name of the meta data field.
#' @param value The value to set for the meta data field.
#'
#' @returns The modified [experiment()] object.
#' @export
set_meta_data <- function(exp, x, value) {
  stopifnot(class(exp) == "glyexp_experiment")
  exp$meta_data[[x]] <- value
  exp
}


#' @rdname set_meta_data
#' @export
set_exp_type <- function(exp, value) {
  stopifnot(class(exp) == "glyexp_experiment")
  checkmate::assert_choice(value, c("glycomics", "glycoproteomics"), null.ok = TRUE)
  exp$meta_data$exp_type <- value
  exp
}


#' @rdname set_meta_data
#' @export
set_glycan_type <- function(exp, value) {
  stopifnot(class(exp) == "glyexp_experiment")
  checkmate::assert_choice(value, c("N", "O"), null.ok = TRUE)
  exp$meta_data$glycan_type <- value
  exp
}
