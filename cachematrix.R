## Put comments here that give an overall description of what your
## functions do

## This function returns a list of functions used to manage the matrix
## content and the memorized inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL               # Contains the memorized inverse matrix
    set <- function(y){     # Reassign matrix
        x <<- y
        m <<- NULL           # Memorized value is not valid anymore
    }
    get <- function() x     # Return matrix content
    setinv <- function(inv) m <<- inv  # Set a new memorized value
    getinv <- function() m            # Return memorized inverse
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)             # Provides the interface for the functions
}

## Returns the inverse of the matrix x (created with makeCacheMatrix).
## If the inverse has already been calculated before for the same
## matrix, the computation will not be repeated, but a memorized
## version of the inverse will be returned
cacheSolve <- function(x, ...) {
    m <- x$getinv()    # Get currently memorized value
    ## If a memorized value exists, return it
    if(!is.null(m)){   
        message("returning memorized data")
        return(m)
    }
    ## If no memorized value exists
    data <- x$get()        # Get current matrix represented by x
    m <- solve(data, ...)  # Get the inverse
    x$setinv(m)            # Memorize this inverse
    m                      # Return the inverse
}
