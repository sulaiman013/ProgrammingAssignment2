


makeCacheMatrix <- function(z = matrix()) {
  inv <- NULL
  set <- function(x) {
    z <<- x
    inv <<- NULL
  }
  get <- function() z
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(z, ...) {
  ## Return a matrix that is the inverse of 'z'
  inv <- z$getInverse()
  if (!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  mat <- z$get()
  inv <- solve(mat, ...)
  z$setInverse(inv)
  inv
}

