## These functions give the inverse of a matrix by either calculating it from scratch or returning the cached inverse.

## This first function creates the required list, which includes functions to set & get the matrix and set & get 
## its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinvrs <- function(invrs1) invrs <<- invrs1
    getinvrs <- function() invrs
    list(set = set, get = get,
         setinvrs = setinvrs,
         getinvrs = getinvrs)

}


## This second function calculates the inverse if it has not been previously calculated. If it has been previously 
## calculated, it simply returns the cached value.

cacheSolve <- function(x, ...) {
        
    invrs <- x$getinvrs()
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    
    invrs <- solve(x$get())
    x$setinvrs(invrs)
    invrs
}

