#' @export
#' @title Kernel Desnity Estimator
#' @author Bjorn Helgason
#' @keywords naive kernel density estimator
#' @usage \code{dense.estimator(x, d, h, method = "naive")}
#' @return Returns the estimated density at points \code{d}, or at min, 1st quantile, median, mean, 3rd quantile and max of the numeric variable, if unspecified.
#' @description \code{dense.estimator} estimates the density of a numeric variable, evaluated at a point.
#' @param x a vector representing a numeric variable.
#' @param d a vector specifying the points of evaluation. If left unspecified, evaluates at min, 1st quantile, median, mean, 3rd quantile and max of the numeric variable.
#' @param h specifies the bandwith used for the estimation. Defaults to Sturges' rule for Naive method and Silverman's for Kernel method.
#' @param method indicates whether teh naive method or the gaussian kernel is used for estimation.

dense.estimator <- function(x, d, h, method = "naive") {
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
  minx <- min(x)
  maxx <- max(x)
  span <- maxx - minx
  if(missing(d)){ # If d not specified, set the defualts
    dDEF <- TRUE
    d <- c(minx, minx + 0.25*span, minx + 0.5*span, mean(x), minx + 0.75*span, maxx)
  } else{dDEF <- FALSE}
  if(!is.vector(d)){
    stop("Expected vector 'd'")
  }
  if(!is.numeric(d)){
    stop("Expected numeric 'd'")
  }
  if(!(method == "naive" || method == "kernel")){
    stop("Unrecognized method. Use 'naive' or 'kernel'.")
  }
  if(missing(h)){
    if(method == "naive"){
      h <- span/(ceiling(log2(length(x))) + 1) # Use Sturges
    }
    if(method == "kernel"){
      h <- sd(x) * (length(x)^(-(1/5))) # Use Silvermans formula
    }
  }
  if(!is.numeric(h)){
    stop("Expected numeric 'h'")
  }
  if(length(h) != 1){
    stop("Expected single length 'h'")
  }

  #Function Execution
  if(method == "naive"){ # Define the weight functions
    K <- function(z){
      return((0.5)*(abs(z) <= 1))
    }
  }else{
   K <- function(z){
      return((1/(sqrt(2*pi)))*exp((-1/2)*z^2))
   }
  }
  pX <- function(z){
    return((1/(length(x)*h))*sum(K((z - x)/h))) #  Then estimate density
  }
  out <- apply(as.array(d), 1, pX) # Apply the estimator function to all inputs d

  cat("Bandwith =", h,"\n")
  if(dDEF){
    mout <- matrix(out, nrow = 6, dimnames = list(c("Min.:", "1st Qu.:", "Median:", "Mean:", "3rd Qu.:", "Max.:"),c("y")))
    print(mout)
    invisible(out)
  }
  else{
    return(out)
  }
  }
