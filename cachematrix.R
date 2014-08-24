##A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(INV) inv <<- INV
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



##cacheSolve will first check whether the matrix's inverse could be fetched from 
##cached data. If not, then compute the inverse matrix, cache it as well as returning it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  INV <- x$getInverse()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  data <- x$get()
  INV <- solve(data) 
  x$setInverse(INV)
  INV
}
