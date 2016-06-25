## set = function that changes the vector stored in the main function
## get = function that returns the vector x stored in the main function
## inv = where inverse is stored
## getinverse = returns the inverse from inv
## setinverse = store value of input into inv 

## makeCacheMatrix makes a special matrix so that an inverse can be taken. 

makeCacheMatrix <- function(x = matrix()) {
      inv = NULL
      set = function(y) {
            x <<- y
            inv <<- NULL
      }
      get = function() x
      setinverse = function(inverse) inv <<- inverse
      getinverse = function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## cacheSolve solves the "matrix" that was created from makeCacheMatrix. If an inverse is already in the cache, it would find it and not recalculate it. 

cacheSolve <- function(x, ...) {
      inv = x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      } else {
      	inv = solve(x$get())
      	x$setinverse(inv)
      	return(inv)
	}
}
