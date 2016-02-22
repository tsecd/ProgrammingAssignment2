## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix sets a local m to null and a an m for the containing 
## environment, which can be used by the cacheSolve function to grab the solved 
## inverse matrix from cacheSolve. If m is stored in the list it has been solved
## and cacheSolve will display the message and return the inverse.  If not the 
## function will take the data calculate the inverse and return the inverse.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y){
      x <<- y
      m <<- NULL
   }
   get <- function()x
   setInverse <- function(solve) m <<- solve
   getInverse <- function() m
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function takes the matrix which ChacheMatrix generated and if the inverse
## matrix has been calculated with print the message and return the inverse matrix
## if not it will calculate the inverse matrix and return it
cacheSolve <- function(x, ...) {
   m <- x$getInverse()
   if(!is.null(m)){
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setInverse(m)
   m
   ## Return a matrix that is the inverse of 'x'
}
