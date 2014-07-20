## These functions were created to meet the requirements of Programming Assignment 2
## of the R Programming Coursera Class, part of the Data Science Specialization
## offered by Johns Hopkins Bloomberg School of Public Health.
## This assignment demonstrates Lexical Scoping and the R Language's ability 
## to cache potentially time-consuming computations.

## This function creates a special "matrix" object that can cache its inverse. 
## It assumes that the input is an invertible matix and does no error handling
## if the matrix is not square.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
      x <<- y
      inv <<- NULL
      }
      get <- function() x
      invertm <- function(solve) inv <<- solve
      getinvertm <- function() inv
      ## This list contains the four methods of the function
      list(set = set, get = get,
              invertm = invertm,
              getinvertm = getinvertm) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves
##the inverse from the cache.

cacheSolve <- function(x, ...) {      
      inv <- x$getinvertm()
      if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$invertm(inv)
      ## Returns a matrix 'inv' that is the inverse of 'x'.
      inv
}
