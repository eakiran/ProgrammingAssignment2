## The functions below store the inverse of a matrix in a cache
## so it doesn't need to be calculated again, reducing
## processing time.

## makeCacheMatrix defines the functions that sets and gets the 
## values of the original matrix it also sets and gets the 
##inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL   }
        get <- function() x
        setInverse <- function(inverseMatrix) inv <<- inverseMatrix
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function checks the existence of a cached inverse and 
## calculates and stores the result of the inverse calculation if 
## none is chached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse() ##retrieve inverse from cache
        if(!is.null(inv)) {   ##checks if value retrieved is not null
                message("getting cached data")
                return(inv)    ##retrieve the cached inverse
                }
        data <- x$get           ##retrieve original matrix
        inv <- solve(data)      ##calculates the inverse
        x$setInverse(inv)       ##caches the inverse
        inv
}
