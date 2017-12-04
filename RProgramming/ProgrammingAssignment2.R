## This program creates a cache for matrix inverse value.
## If requested data is available in the cache it will be
## servered from the cache, otherwise will be computed and
## inserted to cache.

## macheCacheMatrix - Creates the matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) m <<- inverseMatrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## cacheSolve - Returns the inverse of matrix x, from cache if it is already computed.
## If it is not in cache, then it will be computed and inserted into the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}