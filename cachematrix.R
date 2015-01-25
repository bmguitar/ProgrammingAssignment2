## The following functions are designed to speed up the process of getting an inverse of a matrix
## by caching the it rather than computing it repeatedly

## This function creates a special vector to which is a list containing a function to:
## set - set the matrix
## get - return the matrix
## setinverse - set the inverse matrix
## getinverse - return the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
     
     # set the cache to NULL
     m <- NULL
     
     # store matrix
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     # return the matrix
     get <- function() x
     
     # cache the inverse matrix
     setinverse <- function(inverse) m <<- inverse
     
     # return the cached inverse matrix
     getinverse <- function() m
     
     # return a list of above four
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

# This function calculates the inverse of the matrix created with the above function. It checks
# if the inverse has already been calcuated. If so, it returns the inverse from the cache and skips
# the computation.
# Otherwise it calcuates the inverse of the matrix in the cache via the setmatrix function
cacheSolve <- function(x, ...) {
     
     # get the cached inverse matrix
     m <- x$getinverse()
     
     # return the inverse matrix if it isn't NULL
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     # if m is NULL, calculate the inverse matrix and return
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}

