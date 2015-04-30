#' Psychological Antonyms
#'
#' \code{psyAnt} returns a numeric vector, pertaining to the individual's consistency index based
#' upon the correlation between items of a small magnitude.
#'
#' This function is a consistency index based upon correlating all items within a survey, finding
#' the items with the smallest correlations, and then calculating the within item correlation
#' among those measures.
#'
#' @param x An \code{R} dataframe or matrix object.
#' @param cutoff A numeric scalar. The value at which correlations should be declared "antonyms". 
#' Default is set to \emph{r} <= -.60.
#' @param cor.method Character string indicating which method is passed to \code{cor} to use when 
#' calculating the correlation coefficients. 
#' 
#' @return A vector of type numeric.
#' 
#' @export
#' 
#' @seealso \code{\link{careless}}, \code{\link{longString}}, \code{\link{psySyn}}, \code{\link{malDist}}
#' 
psyAnt <- function(x, cutoff = -.60, cor.method = "pearson"){
  checkInput(x)
  cor.matrix <- cor(x, use = "pairwise.complete.obs", method = cor.method)
  if(length(which(cor.matrix <= cutoff)) == 0) stop ("No antonyms found at chosen cut-off level")
  
  cor.matrix[upper.tri(cor.matrix)] <- NA
  diag(cor.matrix) <- NA
  
  antonyms <- which(cor.matrix <= cutoff, arr.ind = TRUE, useNames = FALSE)

  pairs <- cbind(dimnames(cor.matrix)[[1]][antonyms[,1]], # ROWS
                 dimnames(cor.matrix)[[1]][antonyms[,2]]) # COLUMNS
  out <- vector(mode = "numeric", length = nrow(x))
  
  for (i in 1L:nrow(x)){
    matches <- matrix(nrow = nrow(pairs), ncol = 2)
    matches[,1] <- as.numeric(x[i, pairs[,1]])
    matches[,2] <- as.numeric(x[i, pairs[,2]])
    sds <- ifelse(apply(matches, 2, FUN = sd) == 0, TRUE, FALSE)

    if (sum(is.na(sds)) == 2 | sum(sds, na.rm = TRUE) > 0) {
      out[i] <- NA
    } else out[i] <- cor(matches)[2,1]
  }
  return(out)
}  

#' Count Psychological Antonyms
#'
#' \code{countSyn} prints the number of Psychological Antonyms found at a particular cut-off,
#' as well as the variables they pertain to, and their correlation.
#'
#' @param x An \code{R} dataframe or matrix object.
#' @param cutoff A numeric scalar. The value at which correlations should be declared "antonyms". 
#' Default is set to \emph{r} <= -.60.
#' @param cor.method Character string indicating which method is passed to \code{cor} to use when 
#' calculating the correlation coefficients. 
#' 
#' @return A list detailing the number of antonyms found, and their values.
#' 
#' @export
#' 
#' @seealso \code{\link{careless}}, \code{\link{psySyn}}
#' 
countAnt <- function(x, cutoff = -.60, cor.method = "pearson"){
  checkInput(x)
  cor.matrix <- cor(x, use = "pairwise.complete.obs", method = cor.method)
  if(length(which(cor.matrix <= cutoff)) == 0) stop ("No antonyms found at chosen cut-off level")
  if(length(which(cor.matrix <= cutoff)) == 1) stop ("Only 1 pair of antonyms found at chosen cut-off level")
  
  cor.matrix[upper.tri(cor.matrix)] <- NA
  diag(cor.matrix) <- NA
  
  antonyms <- which(cor.matrix <= cutoff, arr.ind = TRUE, useNames = FALSE)
  
  pairs <- cbind(dimnames(cor.matrix)[[1]][antonyms[,1]], # ROWS
                 dimnames(cor.matrix)[[1]][antonyms[,2]]) # COLUMNS
  
  out <- list(Count = nrow(pairs), Variables = data.frame(pairs, Corr = round(cor.matrix[antonyms],3)))
  return(out)
}
