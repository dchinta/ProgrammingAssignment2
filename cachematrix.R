
## x = nxn invertible matrix 
## makeCacheMatrix() and cacheSolve() together return the inverse of a matrix
## and reduce computing time by caching its inverse instead of computing it each time
## the inverse is called. 

## makeCacheMatrix returns a list containing set and get values for the matrix 
## and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## cacheSolve() returns the inverse of a matrix if its not found in the cache 
## and sets the new inverse in the cache matrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
