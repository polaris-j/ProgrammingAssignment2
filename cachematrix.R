### Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
# repeatedly (there are also alternatives to matrix inversion that we will
#             not discuss here). Your assignment is to write a pair of functions that
# cache the inverse of a matrix.

# 1.  `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # Define function to set the value of the matrix. 
  
  set <- function(y) 
  {
    x <<- y # Set the value
    m <<- NULL # Clear the cache
  }
  
  # Define function to get the value of the matrix
  get <- function() x
  
  # Define function to set the inverse
  setInverse <- function (inverse) m <<- inverse
  
  # Define function to get the inverse
  getInverse <- function () m
  
  list(set = set ,
      get = get , 
      setInverse = setInverse ,
      getInverse = getInverse)

}


## 2.  `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse() # Get the catched value
    if (!is.null(m)){ # if not empty
      message("Getting cache data")
      return(m)
    }
    
    # Do folowing if inverse catched value was empty
    # if `X` is a square invertible matrix, then
    # `solve(X)` returns its inverse  
    data <- x$get() # Get value of matrix
    m <- solve(data) # Calculate inverse
    x$setInverse(m) # Cache the result
    m
    
}
