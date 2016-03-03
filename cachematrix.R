## Programming Assignment 2
## These functions store a matrix and cache its inverse.


## This function contains a list of other functions, which can be called to set the value of
## matrix, get the matrix, set the inverse of the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <-function(y){
                x <<- y
                invm <<- NULL
        }
        get <-function() x
        setinvm <- function(inv) invm <<- inv
        getinvm <- function() invm
        list(set = set, get = get, setinvm = setinvm, getinvm = getinvm)
}


## Checks if the inverse of the matrix has been calculated, and if not, solves it and assigns
## it to the cache using the setinvm function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinvm()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinvm(inverse)
        inverse
}
