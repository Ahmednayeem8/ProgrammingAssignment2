makeCacheMatrix <- function(matrix = matrix()) {
  cache <- NULL
  set <- function(matrixInput) {
    matrix <<- matrixInput
    cache$inverse <<- NULL
  }
  get <- function() {
    matrix
  }
  setInverse <- function(inverse) {
    cache$inverse <<- inverse
  }
  getInverse <- function() {
    cache$inverse
  }
  
  cache <- list(set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
  
  cache
}

cacheSolve <- function(matrix, ...) {
  inverse <- matrix$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  mat <- matrix$get()
  inverse <- solve(mat, ...)
  

  matrix$setInverse(inverse)
  inverse
}
