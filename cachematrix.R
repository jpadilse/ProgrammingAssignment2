## Cached matrix inversion functions for programming assignment 2
  ## demonstrates lexical scoping

## Creates a matrix with getters and setters so regetting of an
  ## inverse matrix gets result form cache instead of recalculating

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(mtrx) {
    mat <<- mtrx
    inv <<- NULL
  }
  get <- function() mat
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## Cache solves the inverse of a matrix if already calculated
  ## returns cached results instead

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
