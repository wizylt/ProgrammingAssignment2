## Put comments here that give an overall description of what your
## functions do

## creates special matrix object to cache inverse matrix

makeCacheMatrix <- function(x=numeric()){
  
  m <- NULL
  #We set  the value of matrix
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  #we get the value of matrix
  getmatrix <- function() x
  #we set the inverse matrix
  setinverse<- function(solve) m <<- solve
  #We get the inverse matrix
  getinverse <- function() m
  #creating list of objects
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}



## calculates inverse matrix, stores it and gets it if there is one stored


cacheSolve<- function(x,...){
  #we get the value of m from x
  m <- x$getinverse()
  #we check if we already have inverse matrix, if we do we return the value (with added message)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if we dont have inverse matrix, we calculate it, set and return the value of it
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}