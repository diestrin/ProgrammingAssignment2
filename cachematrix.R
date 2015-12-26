## Used to cache the matrix inversion calculation

## Creates a cache matrix based on another matrix
## This is a middleware for caching

makeCacheMatrix <- function(mtrix = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    mtrix <<- y
    inverse <<- NULL
  }
  
  get <- function() mtrix
  
  setCache <- function(inverseMatrix) inverse <<- inverseMatrix
  
  getCache <- function() inverse
  
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}


## Inverts the matrix or retrive its cached result, if any

cacheSolve <- function(mtrix, ...) {
  inverse <- mtrix$getCache()
  
  if(!is.null(inverse)) {
    return(inverse)
  }
  
  data <- mtrix$get()
  inverse <- solve(data, ...)
  mtrix$setCache(inverse)
  
  inverse
}
