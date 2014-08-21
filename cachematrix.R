## Put comments here that give an overall description of what your
## functions do

## These functions compute the inverse of a square matrix. 
## If the inversion is already in the cache memory it return the inverted matrix skipping the calculation.

## Write a short comment describing this function
## This function create a list of functions:
## 1. set the value of the MATRIX
## 2. get the value of the MATRIX
## 3. set the value of the INVERTED MATRIX
## 4. get the value of the INVERTED MATRIX

makeCacheMatrix <- function(x = matrix()) {
  
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinv <- function(x_inv) z <<- x_inv
  getinv <- function() z
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## The following function first checks to see if the inverted matrix has already been calculated. 
## If so, it gets the inverted matrix from the cache and skips the computation. 
## Otherwise, it return the inversion of the matrix 'x' and sets it in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  z <- x$getinv()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinv(z)
  z
  
}