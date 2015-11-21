## These two functions are used to cache the inverse of a square matrix.

## makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## stores the result of the inversion and sets it to null 
            ## as default if cacheSolve has not yet been used
  set <- function (y) { ## sets the value of the matrix and of the inverse
    x <<- y ## caches the inputted matrix
    m <<- NULL ## caches the value of m to NULL as default
  }
  get <- function() x ## returns the value of the input matrix
  setinverse <- function(inverse) m<<- inverse ## sets the inversed matrix
  getinverse <- function() m ## returns the inversed matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
  ## returns a list with all the previous functions
}


## cachesolve computes the inverse of the "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## gets the inversed matrix, if not already calculated it will be null.
  if(!is.null(m)) { ## if the result of inversion is there
    message("getting cached data") ## prints the message
    return(m) ## and returns the calculated and stored inversion result
  }
  data <- x$get() ## if m is null, it gets the matrix
  m <- solve(data, ...) ## calculates the inverse
  x$setinverse(m) ## runs the setinverse function to cache the result
  m ## returns the result of inversion
}
