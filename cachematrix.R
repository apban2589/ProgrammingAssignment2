## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function - 
##'makeacheMatrix function takes a square matrix as an argument and returns 
## a list of 4 functions: 
##1. 'set' function sets a matrix, 
##2. 'get' function returns the matrix, 
##3. 'setinv' function sets the value of inverse of the matrix
##4. 'getinv' function returns the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  set <- function(y = matrix()) {
    x <<- y
    m <<- matrix()
  }
  
  get <- function(){
    x
  } 
  
  setinv <- function(inv)
  {
    m <<- inv 
  }
  getinv <- function()
  {
    m
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function-
## cacheSolve returns the inverse of a matrix from variable 'm' if that matrix inverse has 
## alreday not been set through setinv function. If the inverse has already been set through setinv function, 
## then it will return the inverse along with the message "Getting cached data".


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!all(is.na(m))){
    message("Getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
    m
}


