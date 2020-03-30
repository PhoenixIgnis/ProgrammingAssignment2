# This pair of functions cache the inverse of a matrix.
# Use exampleMatrix() function to create a test square matrix.
# Use makeCacheMatrix() function to create the special matrix object.
# Use cacheSolve() function to solve the matrix inverse and cache it.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setSolution <- function(solve) m <<- solve
  getSolution <- function() m
  list(set = set, get = get,
       setSolution = setSolution,
       getSolution = getSolution)
  
  
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the 
# inverse from the cache.
cacheSolve <- function(x, ...) {
  
  m <- x$getSolution()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolution(m)
  # Returns a matrix that is the inverse of 'x'
  m
  
}

# Crates a square matrix for testing purposes 
exampleMatrix <- function(n = 5) {
  result <- matrix(data = rexp(100 , rate = 10), nrow = n, ncol = n)
}