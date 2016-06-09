#' @export
#' @title Kernel Desnity Estimate Plot
#' @author Bjorn Helgason
#' @keywords naive kernel density estimator plot
#' @usage \code{dense.plot(x, d, h, method = "naive")}
#' @return No object is returned by this function
#' @description Plots the estimated density of numeric variable 'x', using \code{dense.estimator}.
#' @param x a vector representing a numeric variable.
#' @param d a vector specifying the points of evaluation. If left unspecified, evaluates at min, 1st quantile, median, mean, 3rd quantile and max of the numeric variable.
#' @param h specifies the bandwith used for the estimation. Defaults to Sturges' rule for Naive method and Silverman's for Kernel method.
#' @param method indicates whether teh naive method or the gaussian kernel is used for estimation.
#' @param ... extra parameters to be passed to \code{plot}.

dense.plot <- function(x, n = 500, method = "naive", from, to, ...) {
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
  if(!(method == "naive" || method == "kernel")){
    stop("Unrecognized method. Use 'naive' or 'kernel'.")
  }
  if(!is.numeric(n)){
    stop("Expected numeric 'n'")
  }
  if(length(n) != 1){
    stop("Expected single length 'n'")
  }
  if(missing(from)){
    from <- min(x) - sd(x)
  }
  if(missing(to)){
    to <- max(x) + sd(x)
  }
  if(!is.numeric(from)){
    stop("Expected numeric 'from'")
  }
  if(length(from) != 1){
    stop("Expected single length 'from'")
  }
  if(!is.numeric(to)){
    stop("Expected numeric 'to'")
  }
  if(length(to) != 1){
    stop("Expected single length 'to'")
  }
  if(from >= to){
    stop("Expected 'from' to be less than 'to'")
  }
  #Function Execution
  plot(seq(from = from, to = to, by = (to-from)/n), dense.estimator(x, seq(from = from, to = to, by = (to-from)/n), method = method), type = "l", ylab = "Density", xlab = deparse(substitute(x)),...)
}
