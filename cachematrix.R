## These 2 functions allow to cache the value of a matrix inverse.
## The first function 'makeCacheMatrix' create a new 'type of matrix'. Actually,
## it returns a list containing methods to set and to access the matrix and its
## inverse.
## The second function uses the structure we created with the first function and
## try to retrieve the cached data. If nothing has yet been cached, the function
## will calculate the inverse and cache it before returning it.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inverse) inv <<- inverse
  
  getInv <- function() inv

  list(setMatrix = set, getMatrix = get,
          setMatrixInverse = setInv,
          getMatrixInverse = getInv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv <- x$getMatrixInverse()
    
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    data <- x$getMatrix()
    inv <- solve(data, ...)
    x$setMatrixInverse(inv)
    
    inv

}
