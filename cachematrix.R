## Put comments here that give an overall description of what your
## functions do

## Creates an extended matrix object that can cache its inverse
## ´Four functions on the object: get the matrix, set the matrix, get the inverse and set the inverse.

makeCacheMatrix <- function(x = matrix()) {
  iMtrx <- NULL
  set <- function(y) {
    x <<- y
    iMtrx <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) iMtrx <<- solve
  getinverse <- function() iMtrx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a "cacheMatrix", using cached values where available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iMtrx <- x$getinverse()
  if(!is.null(iMtrx)) {
    message("getting cached data")
    return(iMtrx)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
