
##########################################################################
## makeCacheMatrix returns an object which contains four functions:
## set    : to define the values of a matrix
## get    : to get the values of a matrix
## setinv : computes the inverse of the matrix
## getinv : to get the values of the inverse of the matrix
##########################################################################

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##########################################################################
## cacheSolve returns the inverse of a special matrix:
## if the the inverse has already been calculated, this function returns
## the inverse of the matrix from the cache
##########################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # verify if the inverse matrix exists
  invmat <- x$getinv()
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  } 
  data <- x$get()
  invmat <- solve(data)
  x$setinv(invmat)
  invmat
}
