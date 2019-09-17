# Standardize a vector of numerical data
# data: one column of numeric data
# returns: vector of numeric values, transformed to have mean of zero and 
#          variance of 1
standardize <- function(data) {
  mean.value <- mean(data)
  sd.value <- sd(data)
  z.value <- (data - mean.value)/sd.value
  return(z.value)
}