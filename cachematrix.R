# Assignment 2 - Caching the Inverse of a Matrix

## Assign and Cache Matrix Values and the Inverse of that Matrix
## creating functions to set and get the cached Matrix as well as 
## its inverse.

makeCacheMatrix <- function(x = numeric()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      set_inverse <- function(solve) i <<- solve
      get_inverse <- function() i
      list(set = set, get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}

## Returns Inverse Matrix in Cache or 
## Calculates inverse Matrix if it is not in Cache and returns value

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$get_inverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$set_inverse(i)
      i
}