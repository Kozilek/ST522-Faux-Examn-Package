#' @export
#' @title Chi Square Goodness of Fit
#' @author Bjorn Helgason
#' @keywords Chi-squared Chi Square Goodness of Fit
#' @usage \code{chi.gof(x,p, mehtod = "Monte Carlo")}
#' @return Returns the Chi Square Goodness of Fit value.
#' @description This function performs a Chi Square Goodness of Fit test, estimating the p-value, using the function \code{chi.probability}.
#' @param x a vector containing the counts of the categorical variable.
#' @param p specifies the expected probability of each outcome. If not specified, assumes uniform distribution.
#' @param method indicates the method of computing the p-value. Use \code{"Monte Carlo"} for a Monte Carlo simulation, using \code{chi.probability}, or \code{"base"} for the R base \code{pchisq}.

chi.gof <- function(x, p, method = "Monte Carlo") {
  # Syntax Handling
  if(!is.vector(x)){
    stop("Expected vector 'x'")
  }
  if(!is.numeric(x)){
    stop("Expected numeric 'x'")
  }
  if(length(x) < 1){
    stop("Empty x recieved")
  }
  if (any(x < 0) || anyNA(x)){
    stop("Expected nonnegative and finite 'x' entries")
  }
  if ((sum(x)) == 0){
    stop("Expected at least one entry of 'x' to be positive")
  }
  if(missing(p)){ # If p not specified, let it be uniform.
    p = rep(1/length(x), length(x))
  }
  if(!is.numeric(p)){
    stop("Expected numeric 'p'")
  }
  if(length(p) != length(x)){
    stop("Expected matching length of 'p' and 'x'")
  }
  if(sum(p < 0) > 0){
    stop("Expected positive probabilities in 'p'")
  }
  if(sum(p)!=1){
    stop("Expected total probability in 'p' equal to 1")
  }
  if(!(method == "Monte Carlo" || method == "base")){
    stop("Unrecognized method. Use 'Monte Carlo' or 'base'.")
  }
  #Function Execution
  n <- sum(x)
  TStat <- sum((x - n*p)^2/(n*p)) # Compute the T statistic

  if(method == "Monte Carlo"){
    return(chi.probability(TStat, df = length(x)-1)) # Return the estimated p-value, using chi.probability
  } else{
    return(1-pchisq(TStat, df = length(x)-1)) # Return the estimated p-value, using pchisq
  }


}
