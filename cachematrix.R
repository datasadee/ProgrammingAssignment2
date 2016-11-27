## Put comments here that give an overall description of what your
## functions do

## A special matrix object call cacheMatrix.
## This will store the matrix and cache the inverse value.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix;
        getInverseMatrix <- function() m
        list(set = set, 
             get = get, 
             setInverseMatrix = setInverseMatrix, 
             getInverseMatrix = getInverseMatrix)
}


## Input x: CacheValue class create by makeCacheMatrix function.
## This methods first check the cache value from cacheMatrix.  If it
## is not compute, get the matrix and compute and store the inverse in cacheMatrix
## for reuse purpose.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        if (!is.null(m)) {
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
}
