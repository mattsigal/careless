#' Mahalanobis Distance.
#'
#' \code{malDist} returns a vector indicating the Mahalanobis Distance for each participant in a
#' dataframe.
#'
#' This function is designed for univariate tests, and assumes that all variables passed belong
#' to the same factor. Also, if data are passed as factors, it is converted to integer before
#' calculating the Mahalanobis Distance. If missing data is present, the function will return
#' NA for that observation.
#'
#' @param x An `R` dataframe or matrix object.
#' 
#' @return A vector of type numeric.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' set.seed(77)
#' dat <- matrix(sample(1:3, 200, replace = TRUE),20,10)
#' malDist(dat)
#' }
#' 
#' @seealso \code{\link{longString}}
#' 
malDist <- function(x){
  if (is.matrix(x)){
    x <- as.data.frame(x)
  }

  if (!is.data.frame(x))
    stop("input must be a data.frame or matrix object")
  
  for (i in 1:length(x)){
    if (class(x[,i]) == "factor"){
      x[,i] <- as.numeric(x[,i])
    }
  }

  meanVec <- colMeans(x, na.rm = TRUE)
  varMat <- var(x, na.rm = TRUE)
  out <- unname(mahalanobis(x, meanVec, varMat))
  return(out)
}
