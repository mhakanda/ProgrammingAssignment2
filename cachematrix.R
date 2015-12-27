# The following function "makeCacheMatrix" creates a special "matrix" object 
#which is in fact a list, that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
# The following function computes the inverse of the special "matrix" returned by 
# the "makeCacheMatrix" function. If the inverse has already been calculated (and the 
# matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached Matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
