## These two functions, when called together complute the inverse of
## a matrix, but before computing it's inverse will check the cache
## to determine if the inverse has already been computer, in order
## to save time and processing power

## This function creates a special matrix object that can cache it's 
## inverse value

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the inverse of the matrix x (assuming that x
## is invertable). x is the special matrix created by the makecachematrix
## function above. If the inverse already exist in the cache, it will
## not bother recomputing the inverse, and return the inverse stored 
## in the cache

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
