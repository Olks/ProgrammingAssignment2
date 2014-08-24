## Assignment 2 
## R Programming
## Coursera.org
## Author: Olks (Aleksandra Kocot)
## A pair of functions that cache the inverse of a matrix,
## which need a time-consuming computations.

## First function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Second function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
