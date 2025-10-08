## makeCacheMatrix receives a matrix input and returns a
## list of functions to allow the user to cache the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) cache <<- inv
    getInverse <- function() cache
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve receives a cacheable matrix input and returns
## the inverse of the matrix. If the inverse of the matrix
## has been previously calculated and cached, it will
## return the cached inverse instead of recalculating it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    inv
}

