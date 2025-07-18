% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment.R
\name{experiment}
\alias{experiment}
\title{Create a new experiment}
\usage{
experiment(expr_mat, sample_info, var_info, exp_type, glycan_type, ...)
}
\arguments{
\item{expr_mat}{An expression matrix with samples as columns and variables as rows.}

\item{sample_info}{A tibble with a column named "sample", and other
columns other useful information about samples,
e.g. group, batch, sex, age, etc.}

\item{var_info}{A tibble with a column named "variable", and other
columns other useful information about variables,
e.g. protein name, peptide, glycan composition, etc.}

\item{exp_type}{The type of the experiment, "glycomics" or "glycoproteomics".}

\item{glycan_type}{The type of glycan, "N" or "O".}

\item{...}{Other meta data about the experiment.}
}
\value{
A \code{\link[=experiment]{experiment()}}. If the input data is wrong, an error will be raised.
}
\description{
The data container of a glycoproteomics or glycomics experiment.
Expression matrix, sample information, and variable information
are required then will be managed by the experiment object.
It acts as the data core of the \code{glycoverse} ecosystem.

The \code{glyexp} package provides a set of functions to create,
manipulate, and analyze \code{\link[=experiment]{experiment()}} objects in a tidyverse style.
}
\section{Requirements of the input data}{
\strong{Expression matrix:}
\itemize{
\item Must be a numeric matrix with \strong{variables as rows} and \strong{samples as columns}.
\item The \strong{column names} must correspond to sample IDs.
\item The \strong{row names} must correspond to variable IDs.
}

\strong{Sample information (\code{sample_info}):}
\itemize{
\item Must be a tibble with a column named "sample" (sample ID).
\item Each value in "sample" must be unique.
\item The set of "sample" values must match the column names of the expression matrix (order does not matter).
}

\strong{Variable information (\code{var_info}):}
\itemize{
\item Must be a tibble with a column named "variable" (variable ID).
\item Each value in "variable" must be unique.
\item The set of "variable" values must match the row names of the expression matrix (order does not matter).
}

The function will automatically reorder the expression matrix
to match the order of "sample" and "variable" in the info tables.
}

\section{Meta data}{
Other meta data can be added to the \code{meta_data} attribute.
\code{meta_data} is a list of additional information about the experiment.
Two meta data fields are required:
\itemize{
\item \code{exp_type}: "glycomics" or "glycoproteomics"
\item \code{glycan_type}: "N" or "O"
}

Other meta data will be added by other \code{glycoverse} packages for their own purposes.
}

\section{Index columns}{
The \strong{index columns} are the backbone that keep your data synchronized:
\itemize{
\item The "sample" column in \code{sample_info} must match the column names of \code{expr_mat}.
\item The "variable" column in \code{var_info} must match the row names of \code{expr_mat}.
}

These columns act as unique identifiers,
ensuring that your expression matrix, sample information, and variable information always stay in sync,
no matter how you filter, arrange, or subset your data.
}

\examples{
expr_mat <- matrix(runif(9), nrow = 3, ncol = 3)
colnames(expr_mat) <- c("S1", "S2", "S3")
rownames(expr_mat) <- c("V1", "V2", "V3")
sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"), group = c("A", "B", "A"))
var_info <- tibble::tibble(variable = c("V1", "V2", "V3"), protein = c("P1", "P2", "P3"))
experiment(
  expr_mat, sample_info, var_info,
  exp_type = "glycoproteomics",
  glycan_type = "N"
)

}
