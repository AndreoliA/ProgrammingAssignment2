##Thisfunction, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the matrix
## get the matrix
## set the inverse of the input matrix
## get the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) Inv <<- inverse
  getinv <- function() Inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if 
## the inverse matrix has already been calculated. If so, it gets the
## inverse matrix from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and sets the inverse matrix 
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getinv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setinv(Inv)
  Inv
}

