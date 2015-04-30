#' Careless Summary
#'
#' \code{careless} is a convenience function that calls the various methods within the package
#' and, by default, appends them onto the original dataframe. This function is designed to work with 
#' dataframe or matrix objects.
#'
#' @param x An \code{R} dataframe or matrix object.
#' @param append A boolean scalar. If \code{append = TRUE} the original dataframe is returned with the
#' careless responding metrics bound to it. If \code{append = FALSE}, only the careless responding
#' metrics are returned.
#' @param na.rm A logical scalar. This is passed to \code{longString()}.
#' 
#' @return A dataframe object.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' set.seed(77)
#' dat <- matrix(sample(1:3, 200, replace = TRUE),20,10)
#' stats <- careless(dat, append = FALSE)
#' dat <- careless(dat, append = TRUE)
#' }
#' 
#' @seealso \code{\link{longString}},\code{\link{malDist}}

careless <- function(x, append = TRUE, na.rm = FALSE){

  checkInput(x)
 
  longString <- longString(x, na.rm, return.value = FALSE)
  longStringValue <- longString(x, na.rm, return.value = TRUE)
  MahalanobisD <- malDist(x)
  psyAntonyms <- psyAnt(x)
  psySynonyms <- psySyn(x)
  
  out <- data.frame(longString, longStringValue, MahalanobisD, 
                    psyAntonyms, psySynonyms)
  
  if (append) return(data.frame(x, out)) else return(out)
}
