## This script is made to reduce the computation cost in calculating 
## the inverse of a matrix. 
## That's why we need to cach the result instead of doing repeteadly,
## To doing that, we are implementing the two folloving functions 
## (makeCacheMatrix and cacheSolve).

## Creates a matrix which cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inv <- function(inverse) m <<- inverse
  get.inv <- function() m
  list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}


## computes the inverse of the matrix and checks if the inverse exist and 
## skips the computation.

cacheSolve <- function(x, ...) {
  m <- x$get.inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$set.inv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}