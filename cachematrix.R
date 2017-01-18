## These pair of functions cache the inverse of a matrix.
## This makeCacheMatrix function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     invrs <- NULL
     set <- function(y){
          x <<- y
          invrs <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) invrs <<- inverse
     getInverse <- function() invrs
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     invrs <- x$getInverse()
     if (!is.null(invrs)) {
          message("Fetching inverse matrix from cache")
          return(invrs)
     }
     else{
          message("Computing the inverse matrix")
          inverseMatrix <- x$get()
          invrs <- solve(inverseMatrix)
          x$setInverse(invrs)
          return(invrs)
     }
}
