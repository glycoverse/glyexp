% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samples-and-variables.R
\name{samples}
\alias{samples}
\alias{variables}
\title{Get Samples or Variables of an Experiment}
\usage{
samples(exp)

variables(exp)
}
\arguments{
\item{exp}{An \code{\link[=experiment]{experiment()}}.}
}
\value{
A character vector of sample or variable names.
}
\description{
Getting the names of samples or variables of an \code{\link[=experiment]{experiment()}}.
Syntax sugar for \code{colnames(exp$expr_mat)} and \code{rownames(exp$expr_mat)}.
}
\examples{
exp <- toy_experiment()
samples(exp)
variables(exp)

}
