#' Mahalanobis Distance.
#'
#' \code{malDist} returns a vector indicating the Mahalanobis Distance for each participant in a
#' dataframe.
#'
#' By default, this function is designed for univariate tests, and assumes that all variables 
#' passed belong to the same factor. However, see \code{nitems} if there is a multidimensional
#' structure. Note: if the data are passed as factors, it is converted to integer before
#' calculating the Mahalanobis Distance. If missing data is present, the function will return
#' \code{NA} for that observation.
#'
#' @param x An \code{R} dataframe or matrix object.
#' @param nitems An integer. If dataset is multivariate, indicate the number of items per scale.
#' At present, this is only defined for factors assessed with the same number of indicators.
#' 
#' @return A vector of type numeric. If \code{nitems} is given, the return is of type matrix and contains
#' one vector per factor (D1...Dn), as well as their average (AverageD).
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' set.seed(77)
#' dat <- matrix(sample(1:3, 200, replace = TRUE),20,10)
#' malDist(dat)
#' malDist(dat, nitems = 5)
#' }
#' 
#' @seealso \code{\link{longString}}
#' 
malDist <- function(x, nitems = NA){
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

  if (is.na(nitems)){
    # Default, nitems is NA:
    meanVec <- colMeans(x, na.rm = TRUE)
    varMat <- var(x, na.rm = TRUE)
    out <- unname(mahalanobis(x, meanVec, varMat))
    return(out)
  } else {
    # If nitems is given:
    out <- matrix(NA, nrow = nrow(x), ncol = (ncol(x)/nitems))
    itemGroups <- matrix(data = 1:length(x), nrow = nitems)
    for (i in 1:ncol(itemGroups)){
      subsetDat <- x[,itemGroups[,i]]
      meanVec <- colMeans(subsetDat, na.rm = TRUE)
      varMat <- var(subsetDat, na.rm = TRUE)
      out[,i] <- unname(mahalanobis(subsetDat, meanVec, varMat))
    }
    out <- cbind(out, rowMeans(out))
    colnames(out) <- c(paste0("D", 1:(ncol(out)-1)), "AverageD")
    return(out)
  }
}
