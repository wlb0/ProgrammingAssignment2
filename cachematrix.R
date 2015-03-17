## Assignment 2 - a pair of functions that cache the inverse of a matrix
## Assumes that the supplied matrix is always invertible

## create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL           # initialise object to store cache of inverse of matrix
    set <- function(y) {
        x <<- y         # update matrix object to new value
        m <<- NULL      # clear the cached inverse when (new) matrix is set
    }
    get <- function() x                     # return matrix object
    setinv <- function(solve) m <<- solve   # update cache with solved inverse
    getinv <- function() m                  # return cached inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already calculated (in cache),retrieve inverse from cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()     # get cached matrix inverse
    
    if(!is.null(m)) {   # check that an inverse is cached (ie cache is not null)
        message("getting cached data")
        return(m)       # return cached inverse
    }
    
    # where there's no cached inverse, solve for inverse
    data <- x$get()         # get original matrix
    m <- solve(data, ...)   # solve for inverse of matrix
    x$setinv(m)             # update cache with solved inverse
    m                       # return solved inverse
}
