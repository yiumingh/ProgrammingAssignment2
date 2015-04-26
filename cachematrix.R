### This file creates a cache matrix that computes the Inverse Matrix,
### and stores it somewhere in memory via lexical scoping. The solving
### function in this file is aware of these caching mechanisms, unlike 
### the native solve() for matrices given by the standard library.

# makeCacheMatrix
# creates setters and getters for a matrix that caches its inverse matrix

# INPUTS
# -------------------
# x : matrix()

# OUTPUTS
# -------------------
# list of functions: get, set, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
  # Cached Inverse object
  Im <- NULL
  
  # Setter
  set <- function(y) {
    x <<- y
    Im <<- NULL     # Setting should set the Inverse Matrix to NULL
  }
  
  # Getter
  get <- function() x
  
  # Set Inverse function
  setInverse <- function(Inverse) Im <<- Inverse
  
  # Get Inverse function
  getInverse <- function() Im
  
  # List primitive that has our setters and getters
  list(set = set,
       get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}

# cacheSolve
# It is aware of the cacheMatrix caching mechanism and will take
# advantage of the stored inverse matrix in the computation.

# INPUTs
# -------------------
# x : cacheMatrix list

# OUTPUTS
# -------------------
# Im : Inverse Matrix

cacheSolve <- function(x, ...) {
  Im <- x$getInverse()
  if(!is.null(Im)) {
    message("Using cached inverse...")
    return(Im)
  }
  data <- x$get()
  Im <- solve(data)
  x$setInverse(Im)
  Im
  
}
