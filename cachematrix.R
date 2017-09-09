## The functions aim to save the costly computation of matrix inversion.
## The problem is solved by caching the inverse of a matrix rather than computing repeatedly.

## The following function creates a matrix object that caches its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list (set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse)
  
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("retrieve inverse from the cache")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
