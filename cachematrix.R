## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly (there
## are also alternatives to matrix inversion that we will not discuss here). The
## following functions cache and compute the inverse of a matrix

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() computes computes the inverse of the special "matrix" created by
## returned by makeCacheMatrix() above. In case the inverse has already been
## calculated, then it should bring back the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
