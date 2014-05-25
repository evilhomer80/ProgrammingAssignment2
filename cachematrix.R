## These functions will take a matrix as input and invert the matrix
## If it finds the matrix has not changed it will get the result from a cache to save time

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      myCache <- NULL
      
      ## Set value to that passed and clear the cache
      set <- function(y) {
            x <<- y
            myCache <<- NULL
      }
      
      ## This just returns the value in x
      get <- function() x
      
      ## Store the value in cache
      setCache <- function(solve) myCache <<- solve
      
      ## Get the value back out of cache
      getCache <- function() myCache
      
      ## Constructs the functions for use in makeCacheMatrix
      list(set = set, get = get, setCache = setCache, getCache = getCache)
      
}


## cacheSolve is going to get the value from cache if it exists else perform the Solve function

cacheSolve <- function(x, ...) {
      ## Get the cache value
      myCache <- x$getCache()
      
      ## If there was a cache value we are going to just use that
      if(!is.null(myCache)) {
            message("Getting cached data")
            return(myCache)
      }
      
      ## If no cache 
      
      ## Get the value passed in
      data <- x$get()
      
      ## Work out the inverse on the matrix
      myCache <- solve(data)
      
      ## Now set the cache to the result
      x$setCache(myCache)
       
      myCache
}
