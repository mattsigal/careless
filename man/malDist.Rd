% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/malDist.R
\name{malDist}
\alias{malDist}
\title{Mahalanobis Distance.}
\usage{
malDist(x, nitems = NA)
}
\arguments{
\item{x}{An \code{R} dataframe or matrix object.}

\item{nitems}{An integer. If dataset is multivariate, indicate the number of items per scale.
At present, this is only defined for factors assessed with the same number of indicators.}
}
\value{
A vector of type numeric. If \code{nitems} is given, the return is of type matrix and contains
one vector per factor (D1...Dn), as well as their average (AverageD).
}
\description{
\code{malDist} returns a vector indicating the Mahalanobis Distance for each participant in a
dataframe.
}
\details{
By default, this function is designed for univariate tests, and assumes that all variables
passed belong to the same factor. However, see \code{nitems} if there is a multidimensional
structure. Note: if the data are passed as factors, it is converted to integer before
calculating the Mahalanobis Distance. If missing data is present, the function will return
\code{NA} for that observation.
}
\examples{
\dontrun{
set.seed(77)
dat <- matrix(sample(1:3, 200, replace = TRUE),20,10)
malDist(dat)
malDist(dat, nitems = 5)
}
}
\seealso{
\code{\link{longString}}
}

