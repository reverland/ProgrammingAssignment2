## Put comments here that give an overall description of what your
## functions do

# My functions first make a special matrix and then calculate its inverse.

## Write a short comment describing this function

# In this function, makeMatrix creates a special "matrix", 
# which is really a list containing a function to

# set the value of the vector
# get the value of the vector
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinv <- function(invmax){inv <<- invmax}
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

# The following function calculates the inverse of the special "Matrix"
# created with the above function. However, it first checks to see if the 
# inverse has already been calculated. If so, it gets the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache
# via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
# > a <- makeCacheMatrix(matrix(c(c(2,2,3), c(1,-1,0),c(-1,2,1)), nrow=3, byrow=T))
# > cacheSolve(a)
# [,1] [,2] [,3]
# [1,]    1   -4   -3
# [2,]    1   -5   -3
# [3,]   -1    6    4
# > cacheSolve(a)
# getting cached data
# [,1] [,2] [,3]
# [1,]    1   -4   -3
# [2,]    1   -5   -3
# [3,]   -1    6    4
# looks good