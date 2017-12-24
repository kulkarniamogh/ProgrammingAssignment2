# Functions that cache the inverse of a matrix


# Creation of a matrix object
makeCacheMatrix <- function( m = matrix() ) {
  
  # Set the inverse property
  i <- NULL
  
  # Setter for matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  # Getter for matrix
  get <- function() {
    
    m
  }
  
  # Setter for inverse matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  # Getter for inverse matrix
  getInverse <- function() {
    
    i
  }
  
  # List of methods to be returned
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# "makeCacheMatrix" returns a special matrix. This code computes its inverse
# If the inverse is previously calculated and the matrix is same,
# then "cachesolve" retrieves the previous from cache

cacheSolve <- function(x, ...) {
  
  # Get inverse of matrix
  m <- x$getInverse()
  
  # Check if the inverse is cached
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  # Perform matrix multiplication
  m <- solve(data) %*% data
  
  x$setInverse(m)
  
  # Return statement for matrix
  m
}