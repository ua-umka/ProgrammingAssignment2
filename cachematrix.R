## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## Here presented a pair of functions that cache the inverse of a matrix.

## "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.
##  This "matrix" is really a list containing a function to:
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse matrix
##  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {                                             ##  set the value of the matrix
            x <<- y
            inv <<- NULL
      }
      get <- function() x                                              ##  get the value of the matrix
      setinv <- function(solution) inv <<- solution                    ##  set the value of the inverse matrix
      getinv <- function() inv                                         ##  get the value of the inverse matrix
      list(set = set, get = get, setinv = setinv, getinv = getinv)     ##  return the list
}


## The following function calculates the inverse matrix of the special "matrix" created with the above function.
## However, it first checks to see if the inverse matrix has already been calculated.
## If so, it gets the result from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()                         # query the x matrix's cache
      if (!is.null(inv)) {                      # if there is already a value in the cache
            message("getting cached data")
            return(inv)                         # just return the cached value, no computation needed
      }
      data <- x$get()                           # if there's no value in the cache
      inv <- solve(data, ...)                   # compute inverse matrix
      x$setinv(inv)                             # save the result back to x's cache
      inv                                       # return the result
}
