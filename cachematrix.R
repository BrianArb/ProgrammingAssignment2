# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# 
# makeCacheMatrix, create a special vector for our matrix.
# cacheSolve, create a cache and return the inverse of 'x'.
#
# makeVector creates a special "vector".
#
# makeVector returns a list containing a function to
# set the value of the vector.
# get the value of the vector.
# setinverse the inverse of 'x'.
# getinverse the inverse of 'x'.
#
# example:
# my.matrix <- makeCacheMatrix()
# my.matrix$set(matrix(c(1:4), nrow=2, ncol=2))
# cacheSolve(my.matrix)
# my.matrix$getinverse()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(solve) {
        m <<- solve
    }
    getinverse <- function() {
        m
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
