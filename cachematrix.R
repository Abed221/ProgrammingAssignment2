## Put comments here that give an overall description of what your
## functions do

## makecachematrix function computes the inverse of a matrix for the first time and sets the variable in cached state

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  setMatrix <- function(y) {
    x <<- y
    im <<- NULL
  }
  getMatrix <- function() x
  setinverse <- function(inv) im <<- inv
  getinverse <- function() im
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)

}


## using Cachesolve function to use the data saved in a cached state rather than computing it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if (!is.null(im)) {
    message("getting cached inverse matrix")
    return(im)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setinverse(i)
  i

}
