## This pair of functions caches the inverse of a matrix.
## Matrix inversion is a costly computation so it makes sence to cache the 
## inverse of a matrix rather than compute it repeatedly. 

## makeCacheMatrix function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If matrix has not changed and there is the inverse 
## calculated, function returns the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}