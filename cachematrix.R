# This pair of functions cache the inverse of a matrix.
# Use exampleMatrix() function to create a test square matrix.
# Use makeCacheMatrix() function to create the special matrix object.
# Use cacheSolve() function to solve the matrix inverse and cache it.

# This function creates a special "matrix" object that can cache its inverse.
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the 
# inverse from the cache.
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  # Returns a matrix that is the inverse of 'x'
  m
  
}

# Crates a square matrix for testing purposes 
exampleMatrix <- function(n = 5) {
  result <- matrix(data = rexp(100 , rate = 10), nrow = n, ncol = n)
}