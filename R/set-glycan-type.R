#' Set the glycan type of an experiment
#'
#' Update glycan type metadata and validate the resulting object. Glycan
#' structures and compositions are not changed.
#'
#' @param exp A [GlycomicSE()] or [GlycoproteomicSE()] object. Legacy
#'   [experiment()] objects are also supported with a deprecation warning.
#' @param value A glycan type: `"N"`, `"O"`, `"O-GalNAc"`, `"O-Man"`,
#'   `"O-Fuc"`, `"O-GlcNAc"`, `"O-Glc"`, `"HMO"`, `"GSL"`, `"GAG"`,
#'   `"GPI"`, or `"others"`.
#' @returns The modified object, with the same class as `exp`.
#' @seealso [get_glycan_type()]
#' @examples
#' exp <- as_glycomic_se(real_experiment2)
#' exp <- set_glycan_type(exp, "N")
#' get_glycan_type(exp)
#' @export
set_glycan_type <- function(exp, value) {
  checkmate::assert_multi_class(
    exp,
    c("GlycomicSE", "GlycoproteomicSE", "glyexp_experiment")
  )
  if (is_glycomic_se(exp) || is_glycoproteomic_se(exp)) {
    checkmate::assert_choice(value, c(.valid_glycan_types(), "others"))
    S4Vectors::metadata(exp)[["glycan_type"]] <- value
    methods::validObject(exp)
    return(exp)
  }

  .deprecate_experiment_api("set_glycan_type()")
  .set_meta_data(exp, "glycan_type", value)
}
