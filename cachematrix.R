## Below we have two functions used to calculate and cache the inverse of a matrix.
## "makeCacheMatrix" sets and gets the matrix and its inverse
## "cacheSolve" calculates the inverse and cahes it using "makeCacheMatrix", if it is already cached it will not calculate it



makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL    ## Makes sure the inverse is NULL
  
  ## The below sub-function "set" makes the variables global
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  ## The sub-function "get" returns the matrix
  get <- function() x
  
  ## The sub-function "setinv" sets the inverse as a global variable
  setinv <- function(i) inverse <<- i
  
  ## The sub-function "getinv" returns the inverse
  getinv <- function() inverse
  
  ## The below row lists all the sub-function
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Uses the sub-function "getinv" to get the cached inverse matrix
  cachedInv <- makeCacheMatrix$getinv()
  
   ##If the inverse does exist in the cache it will be returned by the below if-statement
  
  if(!is.null(cachedInv)){
    
    message("Getting chached data")
    return(cachedInv)
  }
  
  ##The two rows calculates the inverse matrix if it doesn't exist and store it with the makeCacheMatrix-function
  
  i <- solve(x, ...)
  makeCacheMatrix$setinv(i)
  
}
