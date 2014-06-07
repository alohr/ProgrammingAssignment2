## The functions makeCacheMatrix() and cacheSolve() implement a caching scheme for the
## inverse of a matrix.
##
## Example usage:
##
## > m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow=2, ncol=2))
##
## > m$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## Get the inverse for the first time:
##
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Any subsequent calls to cacheSolve() with the same matrix as its argument
## will return the previously calculated and cached inverse  matrix:
##
## > cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

makeCacheMatrix <- function(myMatrix = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  
  myInverse <- NULL

  set <- function(m) {
    myMatrix <<- m
    myInverse <<- NULL
  }
  get <- function() myMatrix

  setInverse <- function(inverse) myInverse <<- inverse
  getInverse <- function() myInverse
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned 
  ## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix
  ## has not changed), then the cachesolve should retrieve the inverse from the cache.
  
  myInverse <- x$getInverse()
  if (!is.null(myInverse)) {
    message("getting cached data")
    return(myInverse)
  }
  
  myMatrix <- x$get()
  myInverse <- solve(myMatrix, ...)
  x$setInverse(myInverse)
  myInverse
}
