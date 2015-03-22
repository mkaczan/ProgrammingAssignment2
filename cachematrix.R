###################################
##### R Programming COURSERA ######
###################################

######### Assignment 2 ############


# Write a pair of functions that cache the inverse of a matrix. 

# Write the following functions:
# 1. makeCacheMatrix: 
#    This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" 
#    returned by makeCacheMatrix above. If the inverse has already been 
#    calculated (and the matrix has not changed), then the cachesolve should 
#    retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the solve function 
# in R. For example, if X is a square invertible matrix, then solve(X) returns 
# its inverse.
# For this assignment, assume that the matrix supplied is always invertible.


## Functions that cache the inverse of a matrix.

## makeCacheMatrix() function creates a special "matrix" object that can cache 
## its inverse which is really a list containing a function to set and get the 
## value of the matrix as well as set and get the value of its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)   
}

## cacheSolve() function calculates the inverse of the special "matrix" created with 
## the above function. First it checks whether the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}