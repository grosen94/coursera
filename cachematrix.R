## These functions store a matrix and calculate and
## cache the inverse of the matrix

## makeCacheMatrix creates a matrix and stores the matrix
## as well as its inverse in a lis

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(dim, ...) {
    x <<- matrix(nrow = dim, ncol = dim, ...)
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(erse) inv <<- erse
  getinverse <- function() inv
  list(set = set, get = get, setinverse= setinverse,
      getinverse = getinverse)
}


## cacheSolve will take in a matrix and calculate
## its inverse, or if the inverse has already been
## calculated it will return the cached inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
