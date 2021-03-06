% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ql_matrix.R
\name{ql_matrix}
\alias{ql_matrix}
\title{Contingency Table}
\usage{
ql_matrix(data, vars = colnames(data), p = 0.05, ...)
}
\arguments{
\item{data}{A \code{data.frame} of at least 2 columns.}

\item{vars}{Optional \code{CharacterVector} of target
variables. Default: all variables within \code{data}.}

\item{p}{A \code{NumericVector} of probabilities passed to \code{quantile}. Default: \code{0.05}.}

\item{...}{Additional options passed to \code{quantile}}
}
\value{
A \code{S3} \code{matrix} of class \code{ql_mat}.
}
\description{
Construct a contingency table showing all observed pairs between different states of variables.
}
\details{
For each variable in \code{vars,}ql_matrix\code{first splits the data for each observed state and then calculates the percentile given by}p\code{for each of the remaining variables. So yes, this is not a contingency table. Instead, the default value of}p`
is such that we're almost calulating the minimum value observed
for all variables for each state of a target variable. What
does this tell us? I have no idea.
}
\examples{
x <- data.frame(a = c(0, 1), b = c(1, 1))
ql_matrix(x)

}
