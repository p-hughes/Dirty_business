## A function to find the Euclidean distance between a data.frame of matrix of points and a set point.
# INPUTS : points    - A matrix or dataframe of points where each column represents a dimension
#        : refpoints - A set point. The function finds the Euclidean distance between every point in
#                      points and this point.

point_euclid <- function(points, refpoint, .message=TRUE){
  
  #If points is only one point, makes it a matrix
  if(NCOL(points)==1L) points <- matrix(points, nrow=1L)
  
  #If you don't provide a refpoint, it assumes you mean  the point (0, 0, ..., 0).
  if(missing(refpoint)){refpoint <- rep(0L, ncol(points))
                        if(.message) message("refpoint missing: Assuming origin")}
  #
  if(!is.data.frame(refpoint) && !is.numeric(refpoint)) stop("refpoint is not numeric or dataframe")
  if(is.data.frame(refpoint)) refpoint <- as.matrix(refpoint)
  
  #The workhorse
  sqrt(rowSums((points - matrix(rep(refpoint, nrow(points)), nrow=nrow(points)))^2L))
  
}

# Examples:
# The distance between (5, 5) and (0, 0):

## point_euclid(c(5, 5))

# The distance between (1, 1), (2, 2), (3, 3) and (5, 5):

## x <- matrix(c(1, 2, 3, 1, 2, 3), ncol=2)

## point_euclid(x, c(5, 5))