#' @export
#' @title Throw Needles for Buffon
#' @author Bjorn Helgason
#' @keywords Buffon Buffon's needle throw
#' @usage throw()
#' @return Returns the amount of needles that hit a line
#' @description Throw a needle and determine whether or not it lands on a line. If PI = TRUE, instead returns an estimate of pi.
#' @param d The distance between lines
#' @param l The length of the needles
#' @param n The amount of needles thrown
#' @param PI TRUE if the function should output an estimate of pi

throw <- function(l = 1, d = 1, n= 1, PI = FALSE) {
  # Syntax Handling
  if(l <= 0){
    stop("Nonpositive needle length, l.")
  }
  if(d <= 0){
    stop("Nonpositive distance, d.")
  }
  if(l > d){
    stop("Needle length greater than line distance")
  }
  if(length(n) != 1){
    stop("Expected single value n")
  }
  if(n <= 0){
    stop("Nonpositive amount of needles thrown, n.")
  }
  if(n != as.integer(n)){
    stop("Noninteger amount of needles thrown, n.")
  }
  if(!is.logical(PI)){
    stop("Expected logical PI")
  }

  # Function exectution
  # To slightly reduce code, I treat needles as orientated, with head and point.
  # Likewise, theta will be the counterclockwise angle, parallel to the lines.
  count <- 0 # Start with no needles hit
  pos <- as.array(runif(n, min = -d/2, max = d/2)) # Determine n distances from the needle head to the nearest line, with negative values being under the line
  ang <- as.array(runif(n, min = 0, max = 2*pi)) # Determine n angles for the needles to hit
  height <- apply(ang, 1, sin) * l # Compute the distance spanned by the needles
  hits <- sum(abs(height + pos) > d, sign(pos) != sign(pos + height)) # Compute the number of hits
  # Note: A needle will hit a line if the head and point distances have different signs, or if their sum exceeds teh distance between lines.
  if(PI){
    PIe <- (2*l/(d*(hits/n)))
    return(PIe)
  } else{
    return(hits)
  }

}
