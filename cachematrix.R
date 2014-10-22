## Put comments here that give an overall description of what your
## functions do

## This function is a slight modification of the example makeVector() function
## It creates an list to hold both a matrix and it's cached version
makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(temp_inverse) inverse <<- temp_inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}



## This function is a slight modification of the example cachemean() function
## Takes a list created by makeCacheMatrix.
## If the inverse is cached, return the cached version.  Otherwise compute, 
## cache and return the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  return(inverse)

}
