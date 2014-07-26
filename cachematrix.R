## gets the inverse of a matrix.
## the matrix inverse will be cached when it is first calculated
## and returned from the cache on subsequent calls

## makeCacheMatrix takes a matrix and has functions attached to the matrix itself
## get() returns the matrix itself
## set() sets the matrix
## setinverse() sets the inverse of the matrix
## getinverse() gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## takes in the makeCacheMatrix type and checks if the inverse has already been set
## if it has not been set it sets the inverse using the solve() function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
