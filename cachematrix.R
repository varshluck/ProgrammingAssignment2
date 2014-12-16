## Put comments here that give an overall description of what your
## functions calculates the inverse of a matrix in dynamic state so that if 
## matrix is changes then we calculate new inverse otherwise we use cache data to set the invese value of a matrix

## Write a short comment describing this function
##the first function makeCacheMatrix creates a "vector", which is a 
##list containing a funtion to
##1)set the value of matrix
##2)get the value of matrix
##3)set the value of inverse
##4)get the value of inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##First we try to get the inverse set from the previous function
##if its is a null value we refer to the cached memory that holds value of inverse
##We take the matrix and find its new inverse and set the new inverse so that it is used as cache data next time

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
