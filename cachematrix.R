## Assignment 2 - a pair of functions that cache the inverse of a matrix
## Assumes that the supplied matrix is always invertible

## create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL      # clear the inverse when (new) matrix is set
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already calculated (in cache),retrieve inverse from cache.

cacheSolve <- function(x, ...) {
    m <- x$get()            # cached matrix
    minv <- x$getinv()      # cached matrix inverse
    
    if(!is.null(minv)) {    # check that an inverse is cached
        message("getting cached data")
        return(minv)        # return cached inverse
    }
    
    # where there's no cached inverse, solve for inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m                       # return solved inverse
}
