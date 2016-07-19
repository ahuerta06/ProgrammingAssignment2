## These functions get the inverse of a matrix, creating a cache of the result.
## With this, the performance is improved if we need to use the inverse in other 
## parts of our code.

# Creates a list with 4 methods to set and get the original matrix
# and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    
    set_inv <- function(inv) mat <<- inv
    
    get_inv <- function() mat

    list(set = set
         , get = get
         , set_inv = set_inv
         , get_inv = get_inv)
    
}


# This function calculates the inverse of the matrix. If it has already been 
# calculated, it reads the result from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$get_inv()
    
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$set_inv(mat)
    mat
}
