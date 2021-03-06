% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{collapse}
\alias{collapse}
\title{Collapse a data frame}
\usage{
collapse(data, include_na = FALSE)
}
\arguments{
\item{data}{A data.frame. Data to collapse}

\item{include_na}{Boolean. Whether to collapse consecutive NAs.}
}
\value{
A \code{data.frame} of row size: \code{0 < N <= nrow(data)}.
}
\description{
Collapse unchanging consecutive observations for a given data frame.
}
\details{
If the given data frame contains multiple columns, then
an observation is only collapsed if all of the columns
are unchanging.\preformatted{     Row and column names will be preserved; however, other
     user defined attributes will be dropped.
}
}
\section{Warning}{
 \code{collapse} currently only works with REALSXP,
INTSXP, STRSXP, LGLSXP objects. This covers all of the
common atomic R data types, including more complex types
like \code{Date}. Unfortunately however, this means that
\code{collapse} will return an error if a column has a class
of \code{raw}. Bummer.
}

\examples{
x <- data.frame(x = c(1, 1, 1), y = c(1, 2, 2))
collapse(x)

y <- data.frame(x = c(1, 1, 1), y = c("a", "a", "b"), stringsAsFactors = FALSE)
collapse(y)

}
