#' @export
#' @title Bivariate bootstrap estimator of correlation, standard error, bias and confidence interval.
#' @author Bjorn Helgason
#' @keywords bootstrap correlation bias standard error confidence interval
#' @usage \code{bootbivar(n, x, y)}
#' @return Correlation, estimated correlation, standard error, bias and 95% confidence interval of variables x and y.
#' @description Applying a simple bootstrap estimate, this function estimates the correlation of variables x and y, as well as standard error, bias and confidence interval.
#' @param x a vector containing observations of the first variable
#' @param y a vector containing observations of the second variable
#' @param n The amount of bootstrap replications

bootbivar <- function(n, x, y, print = TRUE) {
  # Syntax Handling
  if(length(x) != length(y)){
    stop("Lengths of x and y don't match")
  }
  if(n < 1){
    stop("Nonpositive n.")
  }
  if(n != as.integer(n)){
    stop("Noninteger amount of bootstra samples, n.")
  }

  # Function exectution

  cor <- cor(x,y) # Computing the correlation
  pair <- function(i){c(x[i], y[i])} # Create a function for pairing data from x and y
  corlist <- numeric(n)
  for(i in 1:n){
    smp <- as.array(sample(1:length(x), length(x) , replace = TRUE)) # Generate a sample of indicies from the range of the data sets
    plist<- apply(smp, 1, pair) # Pair the data from x and y, based on the sampled indicies.
    corlist[i] <- cor(plist[2,],plist[1,]) # Compute the correlation of the sampled pairs.
  }
  estcor <- mean(corlist) # Estimate the correlation as the mean of the bootstrap samples
  se <- sd(corlist)/sqrt(n) # Compute the standard error estimate of the estimated correlation
  erMarg <- 1.96 * se # Compute the estimated margin of error for a 95% confidence interval
  CI <- estcor + c(-erMarg, erMarg) # Estimate the confidence interval of the estimate of the correlation
  bias <- estcor - cor(x,y) # Estimate the bias

  if(print){
    cat("Sample correlation =", cor, "\n\nEstimated correlation =", estcor, "\n\nEstimate standard error =", se, "\n\nEstimate 95% confidence interval:\n",CI, "\n\nEstimated bias =", bias)
    invisible(list(cor = cor, estcor = estcor, se = se, CI = CI))
  } else{return(list(cor = cor, estcor = estcor, se = se, CI = CI))}
}
