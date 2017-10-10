## This pair of functions caches the inverse of a matrix. makeCacheMatrix creates a special "matrix" object
## that can cache its inverse. cacheSolve calculates the inverse of the matrix, unless the inverse has already
## been calculated, in which case it returns the cached inverse.

## makeCacheMatrix creates a list that sets the value of the matrix, gets the value of the matrix,
## sets the value of the inversed matrix, and gets the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix, but first it checks to see if the inverse has already been
## calculated. If so, it gets the cached inverse and skips the computation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrixdata <- x$get()
  m <- solve(matrixdata, ...)
  x$setinverse(m)
  m
}
