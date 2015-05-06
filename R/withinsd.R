#' Within Person Standard Deviation
#'
#' \code{withinsd} returns the within person standard deviation for each row of a dataframe.
#'
#' @param x An \code{R} dataframe or matrix object.
#' @param na.rm A boolean string indicating whether cases with missing values should return a standard deviation based upon
#' the present values, or return \code{NA}.
#' 
#' @return A numeric vector with the within person standard deviations.
#' 
#' @export
#' 
#' @seealso \code{\link{careless}}, \code{\link{psySyn}}
#' 
withinsd <- function(x, na.rm = TRUE) {
  x <- checkInput(x)
  wsd <- apply(x, MARGIN = 1, sd, na.rm = na.rm)
  return(wsd)
}
