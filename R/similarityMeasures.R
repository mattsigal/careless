Itempairs <- function(input_data, test_value){  # helper function
  require("Hmisc")
  data_matrix <- as.matrix(input_data, test_value) #cast data to matrix form
  correlations <- rcorr(data_matrix, type = "pearson") #computes correlations between item pairs
  correlation_coefficients <- correlations$r #creates an object with just the correlation coefficients  
  correlation_coefficients[upper.tri(correlation_coefficients, diag = TRUE)] <- 0 #Set upper triangle equal to 0
  item_pairs <- which(correlation_coefficients > test_value, arr.ind = TRUE) #Create an object with the row and column of correlations > test value 
  item_pairs <- cbind(item_pairs, correlation_coefficients[correlation_coefficients > test_value])#Append a column with the paired variable name
  item_pairs <- cbind(item_pairs, colnames(correlation_coefficients)[as.integer(item_pairs[,2])])#Append a column with the variable names for the y column
  x_names <- rownames(item_pairs) #Get a vector of the variable names for the x column of each item pair
  y_names <- item_pairs[,4] #Get a vector of the variable names for the x column of each item pair
  #Creates a matrix of item pairs
  matches <- matrix(nrow = length(x_names), ncol = 2)
  matches[,1] <- x_names
  matches[,2] <- y_names
  return(matches)
}

Syn_for_one <- function(data, matches){  # helper function for a single row of data
  #Looping variables
  x_sum = 0
  y_sum = 0
  
  #changed to "numeric for cross_products"
  cross_products <- vector(mode = "numeric", length = nrow(matches)) #Creates a vector to store the cross product for each item pair. Will be reset in each iteration of the loop
  x_squares <- vector(mode = "numeric", length = nrow(matches))
  y_squares <- vector(mode = "numeric", length = nrow(matches))
  
  for (k in 1:nrow(matches)) {
    cross_products[k] <- data[as.character(matches[k, 1])] * data[as.character(matches[k, 2])]
    x_squares[k] <- data[as.character(matches[k, 1])] * data[as.character(matches[k, 1])]
    y_squares[k] <- data[as.character(matches[k, 2])] * data[as.character(matches[k, 2])]
    x_sum = x_sum + data[as.character(matches[k, 1])]
    y_sum = y_sum + data[as.character(matches[k, 2])]
  }
  
  sum_cp = sum(cross_products)
  sum_squares_x = sum(x_squares)
  sum_squares_y = sum(y_squares)
  x_average = x_sum / nrow(matches)
  y_average = y_sum / nrow(matches)
  
  
  numerator = sum_cp - (nrow(matches) * x_average * y_average)
  d1 = sum_squares_x - (nrow(matches) * (x_average^2))
  d2 = sum_squares_y - (nrow(matches) * (y_average^2))
  denominator = sqrt(d1 * d2)
  if (denominator == 0) {
    denominator = .0000000000001
  }
  psychsyn = numerator / denominator
  
  #if they put same number for all, no variance and NA is the score
  return(psychsyn)
}

#' Psychological Synonyms.
#'
#' \code{psy.syn} returns...
#'
#' This function...
#'
#' @param input_data Dataframe
#' @param test_value A logical scalar. Should missing values (including NaN) be removed?
#' @return A vector of type integer.
#' @family careless responding functions
#' @seealso \code{\link{functionname}}, \code{\link[packagename]{functioname}}
#' 
psy.syn <- function(input_data, test_value){  # parent function
  data <- as.matrix(input_data)
  #Creates a matrix of item pairs
  matches <- Itempairs(data, test_value)
  synonynms <- apply(data, 1, Syn_for_one, matches) #sends one row at a time to Syn_for_one with the needed argument matches
  return(synonynms)
}
