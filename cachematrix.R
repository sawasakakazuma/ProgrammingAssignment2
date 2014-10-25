## Put comments here that give an overall description of what your
## functions do
##
## These functions will cache a matrix ,calculate its inverse and cache it (for later use).
##
## Write a short comment describing this function
##
## This function will cache a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL #I is the inverse matrix
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) I <<- inv
  getInverse <- function() I
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
##
## This function will retrieve the cached inverse matrix.
## If it isn't calculated yet, then it will be calculated and cached.
cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  if(!is.null(I)){
    return(I)
  }
  m <- x$get()
  I <- solve(m)
  x$setInverse(I)
  I
}
