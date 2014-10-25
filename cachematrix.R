## makeCacheMatrix function:
# This function takes a matrix as a parameter,
# and returns a list that contains functions to 
# a. get the matrix 
# b. set the matrix 
# c. set the inverse of the matrix
# d. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function (y) {
    x<<-y
    invM <<- NULL
  }
  get <- function() x
  setInverse <- function(i) invM <<- i
  getInverse <- function() invM
  list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## CacheSolve function:
# cacheSolve function solves and returns inverse of a matrix.
# parameter(x) is a list of functions that allows to
#   - get/set the matrix that is to be used for doing the inverse 
#   - get/set the inverse
# It also verifies if the inverse call was previously made and result was cached.
# if the cache already has an available value, it returns it,
# if not, computes the inverse, caches it and returns the same
cacheSolve <- function(x) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
      #retrieve the cached inverse here
      message("Retrieving cached inverse for this matrix")
      return (inv)
    }
    inm <- x$get()
    inv <- solve(inm)
    x$setInverse(inv)
    inv
}
