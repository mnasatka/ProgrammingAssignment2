## Matrix inversion is usually a costly computation and their may be
## some benefit to caching the inverse of a matrix rather than computing
## it repeatedly. This pair of functions caches the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  setcache <- function(solve) c <<- solve
  getcache <- function() c
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  c <- x$getcache()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setcache(c)
  c
}
