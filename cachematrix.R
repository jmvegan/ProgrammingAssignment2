## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that provides a list L of methods to set
##  a new matrix X (makeCacheMatrix(X)), to get a created Matrix Y ( L$get() returns Y),
##  to set an inverse matrix ( L$setInverse(X^(-1))), and get an inverse matrix ( L$getInverse())

makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(mInv) mInverse <<- mInv
  getInverse <- function() mInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve is a function that access to the previous created list L with makeCacheMatrix function,
## and get the inverse matrix. If the inverse matrix not exits, is calculated with solve(X), saved into
##   mInverse with setInverse(X^(-1)) method, and returned X


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInverse <- x$getInverse()
  if(!is.null(mInverse)) {
    message("Getting cached matrix inverse")
    return(mInverse)
  }
  matrix <- x$get()
  mInverse <- solve(matrix, ...)
  x$setInverse(mInverse)
  mInverse    
}
