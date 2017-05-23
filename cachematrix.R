## These functions cache the inverse of a matrix.  If the inverse has
## already been computed, cacheSolve will retrieve the cached inverse 
## matrix.  Otherwise, it will compute and store the inverse.

## makeCacheMatrix creates a list of functions to store and retrieve a 
## matrix x and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
        
    setminv <- function(mat) m <<- mat
    
    getminv <- function() m
    
    list(set = set, 
         get = get, 
         setminv = setminv, 
         getminv = getminv)
}


## cacheSolve returns the inverse of the matrix stored in x
## x is a list returned by makeCacheMatrix

cacheSolve <- function(x, ...) {

    m <- x$getminv()
    if(! is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setminv(m)
    m
    
}

