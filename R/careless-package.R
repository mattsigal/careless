#' careless: Metrics for identifying careless responding.
#'
#' The careless package provides a variety of metrics for identifying careless responding.
#' This these are drawn from the statistics used by Craig and Meade (2012).
#'
#' @section careless functions: 
#' So far, this package implements the following functions: \code{longString}, which 
#' returns either the count or the value of the longest set of 
#' repititions in a dataframe; \code{malDist}, which calculates the Malahanobis 
#' Distance, \code{psyAnt} and \code{psySyn}, which calculates Psychological
#' Synonyms and Antonyms, and \code{withinsd}, for calculating the within
#' person standard deviation. Using function defaults, the \code{careless} function
#' can run all of the above functions on a dataframe automatically.
#'
#' @name careless-package
#' @title Metrics for identifying careless responding.
#' @author Matthew Sigal \email{matthewsigal@@gmail.com}
#' @docType package
#' @keywords package
NULL

#' 25 Personality items representing 5 factors
#' 
#' A dataset containing 25 personality self-report items taken from the International Personality
#' Item Pool (ipip.ori.org), and packaged with the \code{psych} library in R. Included here
#' for demonstrative purposes; see the vignette for more details.
#' 
#' @format A data frame with 2,800 rows and 28 varialbes:
#' \describe{
#'    \item{A1:O5}{Raw item responses on 25 items, 5 per personality scale}
#'    \item{gender}{Factor with 2 levels, male = 1 and female = 2}
#'    \item{education}{Number of years of high school education, range 1-5}
#'    \item{age}{Age of participant, range 3-86}
#'  }
#'  @source \url{http://cran.r-project.org/web/packages/psych/index.html}
"bfi"
