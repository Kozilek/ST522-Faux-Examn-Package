#' @export
#' @title Chi Square Probability Function
#' @author Bjorn Helgason
#' @keywords Chi-squared probability estimation Monte Carlo
#' @usage \code{chi.probability(x, df = 1, n= 1000)}
#' @return Returns an array with the probability that chi-square exceeds each given x
#' @description This function estimates the probability that a chi-square dostribution exceeds x, using Monte Carlo simulation.
#' @param x specifies the point of evaluation of the distribution function. If a vector, each point is evaluated.
#' @param df specifies the degrees of freedom
#' @param n specifies the amount of simulations used for the Monte Carlo estimation

chi.probability <- function(x, df = 1, n= 1000) {
  # Syntax Handling
  if(!is.vector(x)){
    stop("Expected vector 'x'")
  }
  if(!is.numeric(x)){
    stop("Expected numeric 'x'")
  }
  if(df != as.integer(df)){
    stop("Expected integer 'df'")
  }
  if(n != as.integer(n)){
    stop("Expected integer 'n'")
  }
  if(df < 1){
    stop("Expected positive 'df'")
  }
  if(n < 1){
    stop("Expected positive 'n'")
  }
  if(length(n) != 1){
    stop("Expected single length 'n'")
  }

  # Function exectution
  rval <- numeric(n) # Initialize the variable used for chi
  for(i in 1:df){ # For each degree of freedom
    vnorm <- rnorm(n) # Generate a normally distributed value
    rval <- rval + vnorm^2 # And add its square to rval
  }

  chicdf <- ecdf(rval) # Compute the empirical cdf of rval
  pvalues <- 1 - apply(as.array(x), 1, chicdf) # Apply the ecdf to each x

  return(pvalues)


}
