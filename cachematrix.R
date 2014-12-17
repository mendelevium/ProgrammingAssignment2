## Cache and Return a matrix that is the inverse of 'x'

## Cache a matrix that is the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inv <- function(not_inv) m <<- not_inv
    get_inv <- function() m
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$get_inv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inv(m)
    m
}


## How to use?
## library(Matrix)
## u <- Matrix(rnorm(9),3)
## a <- makeCacheMatrix(u)
## v <- cacheSolve(a)
## verify with
## u %*% v

## optionnaly use 
## a$get() or a$set(w)
## to get or set the stored data

