## Caching the Inverse of a Matrix:
## Matrix inversion can be a lengthy computation and so by caching 
## the inverse of a matrix rather than computing it repeatedly, we save 
## CPU cycles. Below are a pair of functions that are used to create a 
## special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
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


## This function computes the inverse of the matrix created by 
## makeCacheMatrix above. If the inverse has already been calculated and the 
## matrix has not changed then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}


