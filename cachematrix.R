## Cache and Return a matrix that is the inverse of 'x'

## Cache a matrix that is the inverse of 'x'
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse
    set_inv <- function(not_inv) m <<- not_inv
    
    # get the value of the inverse
    get_inv <- function() m
    
    # create a list of index
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    # get the inverse from the cache
    m <- x$get_inv()
    
    # if it already exist, return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # else, get the matrix
    data <- x$get()
   
    # inverse it
    m <- solve(data, ...)
    
    # save it
    x$set_inv(m)
    
    # return it
    m
}


## How to use?
# library(Matrix)
# u <- Matrix(rnorm(9),3)
# a <- makeCacheMatrix(u)
# v <- cacheSolve(a)
## verify with
# u %*% v

## optionnaly use 
# a$get() # or a$set(w)
## to get or set the stored data

