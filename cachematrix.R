## The goal of this assignment is to create a set of functions to calculate
## the inversion of a matrix, and to store the results in a cache.  The 
## purpose of this is to store previously calculated results to make the 
## running of these functions more efficient.

## The purpose of this function it to create a list of functions that get and set
## the value of the calcuted matrix, and get and set the value of the matrix
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function takes the matrix set in the previous function and determines if
## it had previously cached its inversion.  If so, the inversion is printed with
## a message alerting the user to that fact.  If the matrix had not been
## previously inverted, the function then proceeds to do so, and to print the
## results.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
