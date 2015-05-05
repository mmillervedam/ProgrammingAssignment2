# Sample functions for Programming Assignment 2

## makes a special vector object which is really a list of functions that can be 
## performed on the matrix passed to makeVector (including setting the matrix, 
## setting its mean, and retrieving both of those values)
makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

## calculates the (potetially cached) mean of the special vector by checking to 
## to see if a mean has already been stored. This saves time by only running the
## full calculations if the mean has never been calculated before.

cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}
