## R Programming 016: Programming Assignment #2
## Caching the Inverse of a Matrix
##
## This file contains two functions which work together to
## cache the inverse of a matrix. These programs are toy
## examples which illustrate lexical scoping and the superassignment
## function in R, and general principles of object-oriented programming.

## makeCacheMatrix is a function which creates a special "matrix" object
## that has the ability to cache it's inverse. It is actually composed of
## a list of four functions, which do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                        # Set the cached inverse to null for a new matrix
  set <- function(y) {                             # The set() function is another way to update the
    x <<- y                                        # matrix to be inverted.
    m <<- NULL                                     # If we set a new matrix, the cache is set to null
  }
  get <- function() x                              # get() simply returns the matrix
  setInverse <- function(inverse) m <<- inverse    # This function assigns the inverse to m, is used in cacheSolve()
  getInverse <- function() m                       # This returns m, the cached inverse (if it exists)
  list(set = set, get = get,... =                  # This is what makeCacheMatrix returns, a list of the 
       setInverse = setInverse,                    # four previously defined functions
       getInverse = getInverse)                    
}



## cacheSolve either a) returns the cached value of the inverse from the
## "special" matrix created using makeCacheMatrix(), or if the inverse has
## not yet been calculated, b) calculates, caches and returns in the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()                 # if the cached inverse exists, it is assigned to me
  if(!is.null(m)) {                   # if m is not null
    message("getting cached data")    # print this message
    return(m)                         # return the cached inverse
  }                                   # if m is null (no cached value exists)
  data <- x$get()                     # we assign the matrix to data
  m <- solve(data, ...)               # we calculate the inverse and assign to m
  x$setInverse(m)                     # we set the cache to m, using the setInverse function
  m                                   # return the inverse
}

