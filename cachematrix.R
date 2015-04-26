## This file contains two functions which together allow the user
## to calculate and cashe the inverse of a square invertible matrix. 

## This function creates a special matrix object that can cache its inverse. It 
## takes a regular matrix as its argument and returns a list of functions which
## can be performed on this object including setting and getting the mean.

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


## This function checks to see if the inverse matrix has been previously calculated.
## If not, the function calculates (and stores) it. It takes as its argument a
## matrix of the type created by the function above.

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
