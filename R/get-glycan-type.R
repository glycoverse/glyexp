#' Get the glycan type of an experiment
#'
#' Extract the `glycan_type` field from the metadata of a [GlycomicSE()] or
#' [GlycoproteomicSE()] object.
#'
#' @param exp A [GlycomicSE()] or [GlycoproteomicSE()] object. Legacy
#'   [experiment()] objects are also supported with a deprecation warning.
#'
#' @returns A character string giving the glycan type, or `NULL` if the metadata
#'   field is absent.
#' @seealso [S4Vectors::metadata()]
#' @examples
#' get_glycan_type(real_experiment)
#' get_glycan_type(real_experiment2)
#' @export
get_glycan_type <- function(exp) {
  checkmate::assert_multi_class(
    exp,
    c("GlycomicSE", "GlycoproteomicSE", "glyexp_experiment")
  )

  if (is_glycomic_se(exp) || is_glycoproteomic_se(exp)) {
    return(S4Vectors::metadata(exp)[["glycan_type"]])
  }

  .deprecate_experiment_api("get_glycan_type()")
  exp$meta_data$glycan_type
}
