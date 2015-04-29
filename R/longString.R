#' Long String Max.
#'
#' \code{longString} returns the number of repeated values found sequentially within each row of a dataframe.
#'
#' This function is designed to work with a dataframe or matrix object, and has two primary methods
#' of return: the number of the longest set of repetitions, or the value which those repititions
#' utilised.  For example, if a respondent used option A 10 times in a row, `return.values = FALSE`
#' (default) would return 10, while `return.values = TRUE` would return "A".
#'
#' @param x An `R` dataframe or matrix object.
#' @param na.rm A logical scalar. Should missing values be removed?  If TRUE, long string return 
#' will disregard missingness (e.g., `c(2, 3, NA, 3)` will return a long string value of 2, 
#' while FALSE would return 1.
#' @param return.value A logical scalar. Should the value of the longest string be returned? If 
#' vector has no sequential repeats, this will return `NA`.
#' 
#' @return A vector of type integer, or type character if `return.value = TRUE` and input is
#' a set of factors.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' set.seed(77)
#' dat <- matrix(sample(1:3, 200, replace = TRUE),20,10)
#' longString(dat)
#' longString(dat, return.value = TRUE)
#' }
#' 
#' @seealso \code{\link{malDist}}

longString <- function(x, na.rm = FALSE, return.value = FALSE){
  if (is.matrix(x)){
    x <- as.data.frame(x)
  }

  if (!is.data.frame(x))
    stop("input must be a data.frame or matrix object")
  
  findRowMax <- function(myRow, na = na.rm, ret = return.value){
    counter <- 1
    maxCount <- 1
    maxVal <- NA

    #Trim missing values option.
    if (na == TRUE) {
      myRow <- Filter(function(x)!all(is.na(x)), myRow)
    }
    
    for (i in 1:(length(myRow) - 1)) {
      newval <- unname(myRow[i + 1])
      oldval <- unname(myRow[i])
      
      if (identical(newval, oldval)) {
        counter <- counter + 1
        maxVal <- oldval
      }
      
      if (counter > maxCount) {
        maxCount <- counter
        } else if (!identical(newval, oldval)) {
          counter <- 1
        }
    }
    
    if (ret) out <- maxVal else out <- maxCount
    return(out)
    
  }
  
  return(unname(apply(x, 1, findRowMax, return.value)))
}
