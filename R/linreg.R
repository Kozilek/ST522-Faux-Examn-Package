#' @export
#' @title Regression with Matrices
#' @author Bjorn Helgason
#' @keywords matrix linear regression
#' @usage \code{linreg(formula)}
#' @return Returns a list of residual standard error, r square, adjusted r square, f statistic, error degrees of freedom, model degrees of freedom, f p-value and the coefficent table.
#' @description \code{linreg} executes linear regression using matrix regression.
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.

linreg <- function(formula) {
  # Syntax Handling
  if(class(formula) != "formula"){
    stop("Invalid formula recieved")
  }
  form <- eval(formula, parent.frame()) # Grab the formula from the parent scope
  mf <- model.frame(form) # Create a model frame from the formula
  nvar <- dim(mf)[2] # Determine the amount of variables

  # Function exectution
  premat <- as.matrix(cbind(rep(1,dim(mf)[1]), mf[,2:nvar])) # Construct the design matrix
  depmat <- as.matrix(mf[,1]) # Construct the response matrix
  beta <- solve(t(premat) %*% premat) %*% (t(premat) %*% depmat) # Construct the beta matrix
  dimnames(beta)[[1]][1] <- "(intercept)" # Name the intercept

  Yhat <- premat %*% beta # compute estimated response matrix
  Resid <- depmat - Yhat # compute residuals
  ReSE <- sd(Resid)/sqrt(dim(Resid)[2]) # compute residual standard error

  SSM <- sum((Yhat - mean(depmat))^2) # Compute SS accounted for by the model
  SSE <- sum((depmat - Yhat)^2) # Compute SS not accounted for by the model
  SST <- SSM + SSE # Compute total SS
  dfm <- nvar - 1 # Compute degrees of freedom accounted for by the model
  dfe <- dim(depmat)[1] - nvar# Not accounted for
  dft <- dfe + dfm # Total
  MSM <- SSM/dfm # Compute mean square of model
  MSE <- SSE/dfe # Compute mean square of errors
  MST <- SST/dft # Compute total mean square
  Fs <- MSM/MSE # Compute F statistic
  Rs <- SSM/SST # Compute regular old R-square
  ARs <- 1 - MSE/MST # Compute Adjusted R-square
  pval <- 1 - pf(Fs, dfm, dfe) # Compute p-value of Fs

  SEcoef <- MSE * solve(t(premat)%*%premat)
  SEcoef <- diag(SEcoef) # Computing St. Errors for the coefficients
  SEcoef <- sqrt(SEcoef)

  tcol <- beta/SEcoef # Compute t-values

  pcol <- 2*pt(abs(tcol), df = dfe,lower.tail = FALSE)# Compute p-values

  coeff <- cbind(beta, SEcoef, tcol, pcol)# Bind matrices for printing

  # Printing results

  dimnames(coeff)[[2]] <- c("Estimate", "Std. Error", "t-value", "Pr(>|t|)")
  cat("\nCoefficients:\n")
  print(coeff)
  cat("\nResiduals:\n")
  cat("Residual standard error: ", ReSE ," on ", dfe ," degrees of freedom\n")
  cat("Multiple R-squared:  ", Rs ,",", "Adjusted R-squared:  ", ARs, "\n")
  cat("F-Statistic: ", Fs, " on ", dfm ," and ", dfe, "degrees of freedom, ", "p-value:", pval, "\n")

  invisible(list(ReSE = ReSE, Rs = Rs, ARs = ARs, Fs = Fs, dfe = dfe, dfm = dfm, fpval = pval, coefficients = coeff))


}
