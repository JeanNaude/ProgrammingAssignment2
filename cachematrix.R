## These functions allow the caching of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ## set cache of inverse to null
  
  ## set the data of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the data of the matrix
  get <- function() x
  
  ## set the value of the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  ## get the value of the inverse
  getInverse <- function() inv
  
  ## return a list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## This function computes the inverse of the special "matrix" returned by the function makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Assume x is invertible
  
  inv <- x$getInverse() ## fetch the inverse from the cache
  if(!is.null(inv)) {   ## if the inverse exists in the cache, return it
    message("getting cached data")
    return(inv)
  }
  
  ## Inverse is not in the cache; get the matrix
  data <- x$get()
  inv <- solve(data, ...) ## Calculate the inverse
  x$setInverse(inv)       ## Cache the inverse 
  inv                     ## return the inverse
  
}
