## When passed an invertible matrix x, makeCacheMatrix creates an object which 
## can be passed to cacheSolve to either 
## (a) return a previously cached inverse of the matrix if it exists, OR
## (b) invert the matrix, cache said inverse and then return the inverse 

## makeCacheMatrix creates a list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of solve
## 4. get the value of solve
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## cacheSolve checks if the inverse of this matrix exists in the cache
## and returns it if it does. If the inverse of the matrix doesn't exist in the
## cache, it calculates the inverse of the matrix, stores it in the cache and 
## then returns it

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
