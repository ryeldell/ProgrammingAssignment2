## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## This is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {

     x_invert_cache <- NULL

     set <- function(y) {
          x <<- y
          x_invert_cache <<- NULL
     }
     get <- function() x
     setinvert <- function(solve) x_invert_cache <<- solve
     getinvert <- function() x_invert_cache
     list(set = set, get = get,
          setinvert = setinvert,
          getinvert = getinvert)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.
##
## Computing the inverse of a square matrix can be done with the solve function 
## in R. For example, if X is a square invertible matrix, then solve(X) returns 
## its inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     x_invert_cache <- x$getinvert()
     if(!is.null(x_invert_cache)) {
          message("getting cached data")
          return(x_invert_cache)
     }
     data <- x$get()
     x_invert_cache <- solve(data, ...)
     x$setinvert(x_invert_cache)
     x_invert_cache
     
}
