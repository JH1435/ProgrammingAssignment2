

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  SetInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       SetInverse = SetInverse, getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("finding chached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$SetInverse(inv)
  in
}
