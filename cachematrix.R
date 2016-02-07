# cacheMatrix.R
# Authors: Roger D. Peng, Peter D. Burkholder
# License: none
# Notes: pardon the excessive commenting and additional testCacheMatrix
#  function; largely doing this for my own understanding of internals - PDB

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
    getsolution = getsolution,
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
  x$setsolution(m)
  m
}

## testCacheMatrix()
##  returns TRUE if tests pass
testCacheMatrix <- function() {
  # from ?solve man page, create an 8x8 hilbert matrix
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  h8 <- hilbert(8)

  # make a cacheMatrix object
  cache8 <- makeCacheMatrix(h8)

  # solve (and cache the solution)
  solve8 <- cacheSolve(cache8)

  # test for correct answer
  ans1 <- round(solve8 %*% h8, 3)
  stopifnot(all.equal(ans1, diag(8)))

  # retrieve answer from cache, should message:
  #  'getting cached data'
  cachedSolve8 <- cacheSolve(cache8)

  # test again
  ans2 <- round(cachedSolve8 %*% h8, 3)
  stopifnot(all.equal(ans2, diag(8)))

  # return TRUE
  TRUE
}
