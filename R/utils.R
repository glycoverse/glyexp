has_structure_column <- function(exp) {
  "glycan_structure" %in% colnames(exp$var_info)
}


has_structures <- function(exp) {
  !is.null(exp$glycan_structures)
}
