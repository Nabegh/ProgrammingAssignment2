## Put comments here that give an overall description of what your
## functions do
## The following functions compute and cache the inverse of a matrix.

## Write a short comment describing this function
## This function creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  # initialize the initial value
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }

  ### get the value of the matrix
  get <- function() x
  
  ### set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ### get the value of the inverse
  getinverse <- function() inv
  
  # return the list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # get inverse
  inv <- x$getinverse()
  
  # if inverse exists, return cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not, get matrix
  data <- x$get()
  
  # compute inverse of matrix
  inv <- solve(data, ...)
  
  # cache inverse of matrix
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
