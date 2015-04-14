## This file contains 2 functions: makeCacheMatrix and cacheSolve. cacheSolve it
## meant to be used on the return value of makeCacheMatrix. Please see their
## descriptions below

## makeCacheMatrix creates an object that wraps a matrix and has methods to 
## store and retrieve a precomputed cached value. The cacheMatrix supports any 
## computed value, not just the inverse. Only a single value can be stored and
## the cached value is overwritten the next time store is called. There is no
## way to change the wrapped matrix value, instead it is better to create a new
## cacheMatrix.
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    get <- function() x
    store <- function(f) {
        cache <<- f(x)
    }
    getcache <- function() cache
    list(get = get, store = store, getcache = getcache)
}

## This function takes a cacheMatrix and any further arguments to be passed to 
## solve and returns the inverse matrix. Makes use of the cacheMatrix' caching
## capability
cacheSolve <- function(x, ...) {
    value <- x$getcache()
    if (is.null(value)) {
        # Compute inverse
        x$store(function (x) {
            solve(x, ...)
        })
    }
    x$getcache()
}
