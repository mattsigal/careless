#' Long String Max.
#'
#' \code{longString} returns the number of repeated values found sequentially within each row of a dataframe.
#'
#' This function is designed to work with a dataframe or matrix object, and has two primary methods
#' of return: the number of the longest set of repetitions, or the value which those repititions
#' utilised.  For example, if a respondent used option A 10 times in a row, \code{return.values = FALSE}
#' (default) would return 10, while \code{return.values = TRUE} would return "A".
#'
#' @param x An \code{R} dataframe or matrix object.
#' @param na.rm A logical scalar. Should missing values be removed?  If \code{TRUE}, long string return 
#' will disregard missingness (e.g., \code{c(2, 3, NA, 3)} will return a long string value of 2, 
#' while \code{FALSE} would return 1.
#' @param return.value A logical scalar. Should the value of the longest string be returned? If 
#' vector has no sequential repeats, this will return \code{NA}.
#' 
#' @return A vector of type integer, or type character if \code{return.value = TRUE} and input is
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
#' @seealso \code{\link{careless}}, \code{\link{malDist}}

longString <- function(x, na.rm = FALSE, return.value = FALSE){

  checkInput(x)
  
  findRowMax <- function(myRow, na = na.rm, ret = return.value){
    counter <- 1
    maxCount <- 1
    maxVal <- NA

    #Trim missing values option.
    if (na == TRUE) {
      myRow <- Filter(function(x)!all(is.na(x)), myRow)
    }
    
    if (sum(is.na(myRow)) == length(myRow)) return(NA)
    
    for (i in 1L:(length(myRow) - 1L)) {
      newval <- unname(myRow[i + 1L])
      oldval <- unname(myRow[i])
      
      if (identical(newval, oldval)) {
        counter <- counter + 1L
        maxVal <- oldval
      }
      
      if (counter > maxCount) {
        maxCount <- counter
        } else if (!identical(newval, oldval)) {
          counter <- 1L
        }
    }
    
    if (ret) out <- maxVal else out <- maxCount
    return(out)
  }
  
  return(unname(apply(x, 1, findRowMax, return.value)))
}
