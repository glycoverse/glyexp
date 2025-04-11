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

  # Check if function has been called before
  if (!is.null(exp$meta_data$comp_descriptions_added) && exp$meta_data$comp_descriptions_added) {
    cli::cli_alert_info("Composition descriptions already added. Skipping.")
    return(exp)
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

  # Update experiment and mark as added
  exp$var_info <- new_var_info
  exp$meta_data$comp_descriptions_added <- TRUE
  exp
}


#' Add Descriptions to Glycan Structures
#'
#' This function adds columns about glycan structural properties
#' to the variable information tibble.
#' Depending on the glycan type (N-glycan, O-glycan),
#' different columns are added.
#' Current, only N-glycan descriptions are implemented.
#' See [glymotif::describe_n_glycans()] for columns added.
#'
#' To use this function, the [experiment()] object must contain
#' a `glycan_structure` column in the `var_info` tibble.
#' If `add_structures` has already been called,
#' the cached structures are used.
#' If not, `add_structures` is called internally,
#' and parsed structures are added to the [experiment()] object.
#'
#' @param exp An [experiment()] object.
#'
#' @returns The experiment object with the new columns added.
#' @seealso [add_structures()], [glymotif::describe_n_glycans()]
#' @export
#'
#' @importFrom rlang .data
add_struct_descriptions <- function(exp) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  if (!"glycan_structure" %in% colnames(exp$var_info)) {
    cli::cli_abort("Column {.field glycan_structure} not found in {.field var_info}.")
  }
  if (is.null(exp$meta_data$glycan_type)) {
    cli::cli_abort("Column {.field glycan_type} not found in {.field meta_data}.")
  }
  if (exp$meta_data$glycan_type != "N") {
    cli::cli_abort("Only N-glycans are currently supported.")
  }

  # Check if function has been called before
  if (!is.null(exp$meta_data$struct_descriptions_added) && exp$meta_data$struct_descriptions_added) {
    cli::cli_alert_info("Structure descriptions already added. Skipping.")
    return(exp)
  }

  if (is.null(exp$glycan_structures)) {
    cli::cli_alert_info("Structures not found but {.val glycan_structure} column is detected.")
    cli::cli_alert_info("Calling {.fn add_structures}.")
    exp <- add_structures(exp)
    cli::cli_alert_success("Structures added and can be fetched by {.fun get_glycan_structures}.")
  }

  # Add descriptions (only N-glycans are supported)
  glycan_descriptions <- glymotif::describe_n_glycans(exp$glycan_structures)
  new_var_info <- dplyr::left_join(
    exp$var_info,
    glycan_descriptions,
    by = c("glycan_structure" = "glycan")
  )
  exp$var_info <- new_var_info
  exp$meta_data$struct_descriptions_added <- TRUE
  exp
}


#' Add Glycan Descriptions
#'
#' This function adds glycan description columns to the
#' variable information tibble of an [experiment()] object.
#' If structure information is available,
#' both composition and structure descriptions are added.
#' Otherwise, only composition descriptions are added.
#'
#' @details
#' This function is a wrapper around [add_comp_descriptions()]
#' and [add_struct_descriptions()].
#'
#' @param exp An [experiment()] object.
#'
#' @returns The experiment object with the new columns added.
#' @seealso [add_comp_descriptions()], [add_struct_descriptions()]
#' @export
add_glycan_descriptions <- function(exp) {
  # Check if descriptions have been added
  has_comp_desc <- !is.null(exp$meta_data$comp_descriptions_added) && exp$meta_data$comp_descriptions_added
  has_struct_desc <- !is.null(exp$meta_data$struct_descriptions_added) && exp$meta_data$struct_descriptions_added

  if (has_structure_column(exp) && !has_struct_desc) {
    exp <- add_struct_descriptions(exp)
    cli::cli_alert_success("Structure descriptions added.")
  } else if (has_structure_column(exp)) {
    cli::cli_alert_info("Structure descriptions already added. Skipping.")
  }

  if (!has_comp_desc) {
    exp <- add_comp_descriptions(exp)
    cli::cli_alert_success("Composition descriptions added.")
  } else {
    cli::cli_alert_info("Composition descriptions already added. Skipping.")
  }

  exp
}
