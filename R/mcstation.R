#' @export
#' @title Markov Chain Sationary Probability Estimator
#' @author Bjorn Helgason
#' @keywords markov chain stationary probability estimator
#' @usage \code{mcstation(p, k, n)}
#' @return Returns a table of stationary probabilities.
#' @description Estimates the stationary probabilities of a maarkov chain, using simulation.
#' @param p a matrix containing transition probabilities.
#' @param k an integer dictating the initial state of the chain.
#' @param n an integer specifying the number of steps simulated

mcstation <- function(p,k,n){
  # Syntax Handling
  if(!is.matrix(p)){
    stop("Expected matrix 'p'")
  }
  if(dim(p)[1] != dim(p)[2]){
    stop("Expected square '2'")
  }
  if(!is.numeric(p)){
    stop("Expected numeric 'p'")
  }
  if(any(p<0)){
    stop("Probabilities must be positive")
  }
  if(sum(p) != dim(p)[1]){
    stop("Probabilities must sum to 1 for each row in 'p'")
  }
  if(!is.numeric(k)){
    stop("Expected numeric 'k'")
  }
  if(length(k) != 1){
    stop("Expected single length 'k'")
  }
  if(k > dim(p)[1] || k < 1){
    stop("Recieved invalid starting state 'k'")
  }
  if(!is.numeric(n)){
    stop("Expected numeric 'n'")
  }
  if(length(n) != 1){
    stop("Expected single length 'n'")
  }

  # Function Execution
  state <- k # recall the starting state
  m <- dim(p)[1] # Determine the amount of states
  count <- numeric(m) # Start a counting table
  for(i in 1:n){ # n times
    count[state] <- count[state] + 1 # Count up the current state
    uvar <- runif(1) # Generate a random number from a uniform dist
    j<-1 # Starting at the first state
    while(uvar > sum(p[state,1:j]) && j < m){ # Determine if we move from the current state to j
      j <- j+1
    }
    state <- j
  }
  return(matrix(count/n, nrow = 1, dimnames = list(c("Stationary Probability"), seq(1:m))))

}
