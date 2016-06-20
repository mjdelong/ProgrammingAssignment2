
## This function allows one to cache the inverse of a matrix, which
## oftentimes can be costly in terms of computing. Using the following 
## functions, matrices are stored and the inverses are cached. This saves 
## time and allows one to receive results instead of computing them again.

## The makeCacheMatrix function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)  {
          x <<- y
          inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


             

## The cacheSolve function computes the inverse of the special matrix that 
## was created by the previous function, makeCacheMatrix. Once the inverse 
## has already been calculated, it can retrieve the inverse from the cache 
## (as long as the matrix has not changed).

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
      }

