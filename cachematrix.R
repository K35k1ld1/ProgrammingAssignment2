## The aim of the two functions is to cache the result of a matrix 
## inversion computation.

## The makeCacheMatrix create a special vector which contains a list
## of 4 functions:
## - setMat: permit to set the matrix
## - getMat: permit to get the matrix
## - setInverse: permit to set the inverse of the matrix
## - getInverse: permit to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setMat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  getMat <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(setMat = setMat, getMat = getMat,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the original 
## matrix input to makeCacheMatrix(), and store the result in a 
## cache object.

cacheSolve <- function(x, ...) {

  ## Check if the inverse was already compute
  inv <- x$getInverse()
  
  ## if yes, return the value stored in the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
    
  ## if no, compute the inverse... 
  data <- x$getMat()
  inv <- solve(data)
  
  ## and store the result in the cache.
  x$setInverse(inv)
  
  inv  
}
