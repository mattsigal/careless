#' Long string max.
#'
#' \code{LSmax} returns the sum of repeated values.
#'
#' This function...
#'
#' @param ... Dataframe
#' @param na.rm A logical scalar. Should missing values (including NaN) be removed?
#' @return A vector of type integer.
#' @examples
#' LSmax(sample(1:4, 30, replace = TRUE))
#' @family careless responding functions
#' @seealso \code{\link{functionname}}, \code{\link[packagename]{functioname}}

LSmax <- function(myRow){
  counter <- 1
  maxCount <- 1
  #Trim missing values option.
  myRow <- Filter(function(x)!all(is.na(x)), myRow)
  for (i in 1:((length(myRow) - 1))) {
    newval <- as.numeric(myRow[i + 1])
    oldval <- as.numeric(myRow[i])
    if (newval == oldval) {
      counter <- counter + 1
    }
    if (counter > maxCount) {
      maxCount <- counter
    } else if (newval != oldval) {
      counter <- 1
    }
  }
  return(maxCount)
}
