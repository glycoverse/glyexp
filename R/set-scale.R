#' Set the abundance scale of an experiment
#'
#' Update scale metadata without transforming abundance values. The resulting
#' object is validated, so `"raw"` requires non-negative abundance values.
#'
#' @param exp A [GlycomicSE()] or [GlycoproteomicSE()] object.
#' @param value A scale: `"raw"`, `"log"` (natural logarithm), `"log2"`,
#'   `"log10"`, or `"logx"` (a transformed logarithmic or log-like scale).
#' @returns The modified object, with the same class as `exp`.
#' @seealso [get_scale()]
#' @examples
#' exp <- set_scale(real_experiment2, "raw")
#' get_scale(exp)
#' @export
set_scale <- function(exp, value) {
  checkmate::assert_multi_class(exp, c("GlycomicSE", "GlycoproteomicSE"))
  checkmate::assert_choice(value, c("raw", "log", "log2", "log10", "logx"))
  S4Vectors::metadata(exp)[["scale"]] <- value
  methods::validObject(exp)
  exp
}
