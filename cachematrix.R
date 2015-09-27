## makeCacheMatrix creates a list of functions similar to the one
## used in the example (makeVector function):

## cacheSolve returns the inverse of the matrix



## We define m as the "empty" inverse of x and we define following
## functions:
## set(y) - sets y as the value of the matrix and
##          "cleans the cache"
## get() - gets the value of the matrix
## setinv(inv) - sets inv as the value of the inversion
## getinv() - gets the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## The argument x is a list prepaired by makeCacheMatrix.
## First we get cached inversion and assign it to variable m.
## If the cache was not empty, we just return it.
## If it was, we calculate the inversion, set it and return it.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setinv(m)
        m
}
