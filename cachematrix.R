## This file contains two functions which together allow the user to calculate 
## and cache the inverse of a square invertible matrix. 

## This function creates a special matrix object that can cache its inverse. 
## Its argument is a regular matrix and it returns a list of functions which
## can be performed on this object including setting and getting the inverse.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL 
      set <- function (y){
            x <<- y
            inverse <<- NULL
      }
      get <- function () x
      setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
      getinverse <- function() inverse
      list (set = set, get = get, 
            setinverse = setinverse, 
            getinverse = getinverse)
}


## This function returns the inverse of a matrix object created by the function
## above by first checking to see if an inverse has been previously cached.
## If not, the function calculates (and stores) the appropriate inverse.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)){
              message ("getting cached data")
              return (inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
