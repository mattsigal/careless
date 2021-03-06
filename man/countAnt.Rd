% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/psyAnt.R
\name{countAnt}
\alias{countAnt}
\title{Count Psychological Antonyms}
\usage{
countAnt(x, cutoff = -0.6, cor.method = "pearson")
}
\arguments{
\item{x}{An \code{R} dataframe or matrix object.}

\item{cutoff}{A numeric scalar. The value at which correlations should be declared "antonyms".
Default is set to \emph{r} <= -.60.}

\item{cor.method}{Character string indicating which method is passed to \code{cor} to use when
calculating the correlation coefficients.}
}
\value{
A list detailing the number of antonyms found, and their values.
}
\description{
\code{countSyn} prints the number of Psychological Antonyms found at a particular cut-off,
as well as the variables they pertain to, and their correlation.
}
\seealso{
\code{\link{careless}}, \code{\link{psySyn}}
}

