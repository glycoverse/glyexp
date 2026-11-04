#' Get the abundance scale of an experiment
#'
#' Extract the `scale` field from the metadata of a [GlycomicSE()] or
#' [GlycoproteomicSE()] object without inferring or transforming abundance values.
#'
#' @param exp A [GlycomicSE()] or [GlycoproteomicSE()] object.
#'
#' @returns A character string giving the abundance scale: `"raw"`, `"log"`
#'   (natural logarithm), `"log2"`, `"log10"`, or `"logx"` (a transformed
#'   logarithmic or log-like scale). Returns `NULL` if the metadata field is absent.
#' @seealso [S4Vectors::metadata()], [get_glycan_type()]
#' @examples
#' get_scale(real_experiment)
#' get_scale(real_experiment2)
#' @export
get_scale <- function(exp) {
  checkmate::assert_multi_class(exp, c("GlycomicSE", "GlycoproteomicSE"))
  S4Vectors::metadata(exp)[["scale"]]
}
