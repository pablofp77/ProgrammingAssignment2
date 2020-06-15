## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will create a list with functions to set & get a matrix , and set & get the inverse of a matrix
## seems to be building in essence an object of the class matrix with 4 methods 
makeCacheMatrix <- function(x = matrix()) {
  inv_mtrx <- NULL
  
  setMtrx <- function (y){
    x <<- y
    inv_mtrx <<- NULL
  } 
  getMtrx <- function () x
  setInvMtrx <- function (inv) inv_mtrx <<- inv
  getInvMtrx <- function () inv_mtrx
  
  list (set = setMtrx, get = getMtrx, setInvMtrx = setInvMtrx , getInvMtrx = getInvMtrx)
}


## Write a short comment describing this function
## Inside this function that will have a parameter an object matrix , we will calculate the inverse of the object
## matrix if this hasn't been calculated before, if so , it will return the value existing in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mtrx <- x$getInvMtrx()
  if(!is.null(inv_mtrx)) {
    message("getting cached data")
    return(inv_mtrx)
  }
  data <- x$get()
  inv_mtrx <- solve(data, ...)
  x$setInvMtrx(inv_mtrx)
  inv_mtrx
  
}
