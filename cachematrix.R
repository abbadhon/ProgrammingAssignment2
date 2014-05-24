## makeCacheMatrix creates a setters and getters for object of type matrix
## that will be inverted  by other function
##
makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
      x <<- y
      matinv <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) matinv <<- inverse
  getinverse <- function() matinv
# Return list of getter/setter
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the matrix inverse from makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinverse()
  if (is.null(matinv)) {
    matinv <- solve(x$get())
    x$setinverse(matinv)
  } else {
      message("getting cached inverse matrix")
  }  
  return(matinv)
}