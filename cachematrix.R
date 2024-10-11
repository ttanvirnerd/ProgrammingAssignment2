
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  set <- function(y) {
    x <<- y    # Set the matrix
    inv <<- NULL  # Reset the inverse cache
  }
  get <- function() x  # Return the matrix
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
  getInverse <- function() inv  # Retrieve the cached inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  
  if(!is.null(inv)) {  
    message("getting cached data")
    return(inv)  
  }
  data <- x$get()  
  inv <- solve(data, ...)  
  x$setInverse(inv)  
  inv  
}
