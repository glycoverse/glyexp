% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toy-experiment.R
\name{toy_experiment}
\alias{toy_experiment}
\title{Create a Toy Experiment}
\usage{
toy_experiment()
}
\value{
An \code{\link[=experiment]{experiment()}} object.
}
\description{
This function creates a toy experiment object for new users to play with
to get familiar with \code{\link[=experiment]{experiment()}} objects.
The experiment has 4 variables and 6 samples.
\code{var_info} contains fields: \code{protein}, \code{peptide}, and \code{glycan_composition}.
\code{sample_info} contains fields: \code{group} and \code{batch}.
}
