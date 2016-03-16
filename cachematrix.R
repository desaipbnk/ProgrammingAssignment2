## Put comments here that give an overall description of what your
## functions do
## Usage:
## x<-matrix(c(1,3,2,4),2,2)
## y<-makeCacheMatrix(x)
## cacheSolve(y) - should compute the inverse
## cacheSolve(y) - running again should return the cached value

## Write a short comment describing this function
# The following function creates a matrix that caches its inverse
# and contains a list of following functions
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse of the matrix
# 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# The following function provides the the inverse of the matrix
# it return the cached result along with a message if possible 
#   otherwise calculates the inverse
# 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
