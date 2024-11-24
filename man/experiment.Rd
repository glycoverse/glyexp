% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-experiment.R
\name{experiment}
\alias{experiment}
\title{Create a new experiment}
\usage{
experiment(name, expr_mat, sample_info, var_info)
}
\arguments{
\item{name}{A character string for the name of the experiment.}

\item{expr_mat}{An expression matrix with samples as columns and variables as rows.}

\item{sample_info}{A tibble with a column named "sample", and other
columns other useful information about samples,
e.g. group, batch, sex, age, etc.}

\item{var_info}{A tibble with a column named "variable", and other
columns other useful information about variables,
e.g. protein name, peptide, glycan composition, etc.}
}
\value{
A \code{\link[=experiment]{experiment()}}. If the input data is wrong, an error will be raised.
}
\description{
An experiment is a S3 object that contains the data of a
glycoproteomics or glycomics experiment.
Expression matrix, sample information, and variable information
are required then will be managed by the experiment object.

\code{colnames(expr_mat)} should be identical to \code{sample_info$sample},
and \code{rownames(expr_mat)} should be identical to \code{var_info$variable}.
Both "sample" and "variable" columns should be unique.
Order doesn't matter, as the expression matrix will be reordered
to match the order of \code{sample_info$sample} and \code{var_info$variable}.

\code{experiment()} provides multiple methods in tidyverse style to
filter samples or variables and to add new sample information or
variable information.
It is the core of \code{glyexp} ecosystem as the data container.
}
\examples{
expr_mat <- matrix(runif(9), nrow = 3, ncol = 3)
colnames(expr_mat) <- c("S1", "S2", "S3")
rownames(expr_mat) <- c("V1", "V2", "V3")
sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"), group = c("A", "B", "A"))
var_info <- tibble::tibble(variable = c("V1", "V2", "V3"), protein = c("P1", "P2", "P3"))
experiment("my_exp", expr_mat, sample_info, var_info)

}
