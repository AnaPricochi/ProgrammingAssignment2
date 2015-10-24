## A pair of functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.
## The function creates a special "matrix", which is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get matrix
  get <- function() x
  
  # Set inverse
  setinverse <- function(solve) inv <<- solve
  
  # Get inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  
  ## Check if the inverse has already been calculated, and if do get the inverse from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  ## Compute inverse
  m <- solve(data, ...)
  
  ## Cache inverse
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
