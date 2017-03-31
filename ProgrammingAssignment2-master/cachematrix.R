## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #function inside other function is called closure; it inherits from parent
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
#computes the inverse returned by makeCacheMatrix, or retrieves it from cache if computed before.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  #if cashed -> retrieve
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if not cached -> solve
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}