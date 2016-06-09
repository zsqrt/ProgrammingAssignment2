## This R script includes functions that can cache
## the inverse of a given matrix.

## creates matrix, or with an existing matrix,
## provides a list containing functions to manipulate this matrix:
##   set matrix, 
##   get metrix,
##   set matrix inverse,
##   get metrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(value) inverse <<- value
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calculates the inverse of the matrix created
## the above function. It retrieves the inverse value if cached,
## otherwise the inverse is computed and stored in cache for later use.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    m <- x$get()
    inverse <- solve(m, ...)
    x$setInverse(inverse)
    inverse
}
