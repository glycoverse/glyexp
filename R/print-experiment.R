#' @export
print.glyexp_experiment <- function(x, ...) {
  cli::cli_h1("Experiment")
  cli::cli_alert_info("Expression matrix: {.val {ncol(x$expr_mat)}} samples, {.val {nrow(x$expr_mat)}} variables")
  cli::cli_alert_info("Sample information fields: {.field {setdiff(colnames(x$sample_info), 'sample')}}")
  cli::cli_alert_info("Variable information fields: {.field {setdiff(colnames(x$var_info), 'variable')}}")
}
