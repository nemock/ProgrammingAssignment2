# These functions are modeled directly from the assignment example for 
# using a similar function to store a numeric vector and cache its mean
# As Dr Peng says, programmers are lazy so I will do my best to use the 
# example code to its fullest, because why else would they provide it?
# 
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }
# The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
# 
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }
#
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than computing it 
# repeatedly (there are also alternatives to matrix inversion that we 
#             will not discuss here). Your assignment is to write a pair 
# of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#   
#   makeCacheMatrix: This function creates a special "matrix" object that 
# can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the inverse 
# from the cache.
#
# Lucky for me, the function solve() returns the inverse of the matrix x.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 
# First it checks if the inverse is already been computed.
# If yes, it  Retrieves the result  without repeating the computation.
# If no, it computes the inverse, and uses the set inverse function to cache the value and.
# This function assumes the matrix can be inverted (as noted in the assignment)

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
