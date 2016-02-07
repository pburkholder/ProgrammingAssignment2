# cacheMatrix.R
# Authors: Roger D. Peng, Peter Burkholder
# License: none

## makeCacheMatrix(matrix x)
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

  # return the 'list' of functions on this object
  list(
    set = set,
    get = get,
    getsolution = getsolution
    setsolution = setsolution
    )
}

## cacheSolve(x)
##   input: a makeCacheMatrix object
##   returns: m, a solution 'solve()' of input
##     and associates solution to input object
##     for later cache retrieval

cacheSolve <- function(x, ...) {
  # fetch current solution from x (or NULL if not cached)
  m <- x$getsolution()

  # if not NULL, then return the cached value of m
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }

  # extract data from x with get(),
  # then assign matrix inverse to m
  data <- x$get()
  m <- solve(data, ...)

  # set m for later cache hit, and return solution m
  x$set(m)
  m
}
