## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix:
##   input: matrix
##   returns: list of functions on the matrix:
##     - set(x): set the matrix associated with this object
##     - get():  return the matrix
##     - setsolution(function): cache a solution to function with this object
##     - getsolution(): return the cached solution, or NULL if not cached

makeCacheMatrix <- function(x = matrix()) {
  # initalize m, this object's solution, to NULL
  m <- NULL

  # set: reinitializes m to NULL, and resets to the input (x <<- y)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # get: returns the input 'x' matrix OR
  # the 'x' as reset by the 'set' function
  get <- function() x

  # setsolution: associate a solution, func, to this object
  setsolution <- function(func) {
    m <<- func
  }

  # getsolution: return m, no input
  getsolution <- function() m

  # return the 'list' of functions for
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
