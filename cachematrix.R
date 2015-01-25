## Implementation of a matrix object that supports caching its inverse


makeCacheMatrix <- function(x = matrix()) {
  ## A constructor for the matrix with cached inverse
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


cacheSolve <- function(x, ...) {
  ## A function to compute the inverse of 'x' or retrieve the cached solution
  # Operates on matrices constructed whit 'makeCacheMatrix' function
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } else {
    A <- x$get()
    inv <- solve(A, ...)
    x$setinverse(inv)    
  }
  
  inv
}
