#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import glyrepr
#' @import methods
#' @importFrom lifecycle deprecated
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom rlang %||%
#' @importFrom tidyselect all_of
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
## usethis namespace: end
NULL

#' @export
#' @importFrom SummarizedExperiment assay
SummarizedExperiment::assay

#' @export
#' @importFrom SummarizedExperiment `assay<-`
SummarizedExperiment::`assay<-`

#' @export
#' @importFrom SummarizedExperiment rowData
SummarizedExperiment::rowData

#' @export
#' @importFrom SummarizedExperiment `rowData<-`
SummarizedExperiment::`rowData<-`

#' @export
#' @importFrom SummarizedExperiment colData
SummarizedExperiment::colData

#' @export
#' @importFrom SummarizedExperiment `colData<-`
SummarizedExperiment::`colData<-`

#' @export
#' @importFrom S4Vectors metadata
S4Vectors::metadata

#' @export
#' @importFrom S4Vectors `metadata<-`
S4Vectors::`metadata<-`

ignore_unused_imports <- function() {
  glyrepr::glycan_composition
}
