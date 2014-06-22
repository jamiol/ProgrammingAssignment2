## These functions can be used to calculate and cache matrix inverse values.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function will return the inverse of the cache matrix type (makeCacheMatrix).
## If the inverse has previously been calculated the cached value will be returned.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Returning cached inverse")
        inverse
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}
