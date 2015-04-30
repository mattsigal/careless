# Helper Functions

checkInput <- function(x) {
  # Make sure input is either a matrix or a dataframe
  if (is.matrix(x)){
    x <- as.data.frame(x)
  }
  
  if (!is.data.frame(x))
    stop("input must be a data.frame or matrix object")
  
  return(x)
}
