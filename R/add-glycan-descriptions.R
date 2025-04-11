#' Add Descriptions to Glycan Compositions
#'
#' This function adds the following columns to the
#' variable information tibble:
#' - 'n_hex': number of Hex
#' - 'n_hexnac': number of HexNAc
#' - 'n_fuc': number of Fuc
#' - 'n_neuac': number of NeuAc
#' - 'n_neugc': number of NeuGc
#' - 'n_sia': number of Sia
#'
#' `n_sia` is the sum of `n_neuac` and `n_neugc`.
#'
#' @param exp An [experiment()] object.
#'
#' @returns The experiment object with the new columns added.
#' @export
#'
#' @importFrom magrittr %>%
add_comp_descriptions <- function(exp) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  if (!"glycan_composition" %in% colnames(exp$var_info)) {
    cli::cli_abort("Column {.field glycan_composition} not found in {.field var_info}.")
  }

  # Add columns
  extract_n <- function(x, mono) {
    stringr::str_extract(x, paste0(mono, "(\\d+)"), group = 1) %>%
      as.integer() %>%
      tidyr::replace_na(0L)
  }
  new_var_info <- dplyr::mutate(
    exp$var_info,
    n_hex = extract_n(.data$glycan_composition, "H"),
    n_hexnac = extract_n(.data$glycan_composition, "N"),
    n_fuc = extract_n(.data$glycan_composition, "F"),
    n_neuac = extract_n(.data$glycan_composition, "A"),
    n_neugc = extract_n(.data$glycan_composition, "G"),
    n_sia = extract_n(.data$glycan_composition, "S"),
    n_sia = .data$n_sia + .data$n_neuac + .data$n_neugc
  )

  # Update experiment
  exp$var_info <- new_var_info
  exp
}
