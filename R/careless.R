#' Careless Respondants
#'
#' \code{careless} is a summary function that calls various functions within the `careless` package.
#'
#' This function is designed to work with a dataframe or matrix object.
#'
#' @param x An `R` dataframe or matrix object.
#' @param append A boolean scalar. If `append = TRUE`, the original dataframe is returned with the
#' careless responding metrics bound to it. If `append = FALSE`, only the careless responding
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
  if (is.matrix(x)){
    x <- as.data.frame(x)
  }

  if (!is.data.frame(x))
    stop("input must be a data.frame or matrix object")
  
  longString <- longString(x, na.rm, return.value = FALSE)
  longStringValue <- longString(x, na.rm, return.value = TRUE)
  MahalanobisD <- malDist(x)
  
  out <- data.frame(longString, longStringValue, MahalanobisD)
  
  if (append) return(data.frame(x, out)) else return(out)
}
