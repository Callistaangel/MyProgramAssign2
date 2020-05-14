## These functions are designed to save time by creating the inverse of a Matrix
## and then cacheing it somewhere else

## This function is designed to create a special "matrix" object 
## that is able to cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the previous function and finds its inverse.
## It does this by either calcuating the inverse, or if the inverse already exists, 
## pulling it from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  cgoal <- x$get()
  m <- solve(cgoal, ...)
  x$setinverse(m)
  m
}
