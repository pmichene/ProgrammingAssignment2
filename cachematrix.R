## Coursera R Programming Assignment #2
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:

## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
    # Initialixie the second variable (xi) in within the function
    # The variable X is set as a function argument
    xi <- NULL
    set_as <- function(y){
        # set the value of "x" to the input of the function "set_as"
        x <<- y
        # If "xi" had some value, it is reset to NULL
        xi <<- NULL
    }
    get_as <- function() x
    set_asInverse <- function(matrixInverse) xi <<- matrixInverse
    get_asInverse <- function() xi
    # Name each element of the list.  Name = value
    list(set_as = set_as, get_as = get_as, set_asInverse = set_asInverse, get_asInverse = get_asInverse)
}



cacheSolve <- function(x, ...) {
    xi <- x$get_asInverse()
    if(!is.null(xi)) {
        message("grabbing cache data")
        return(xi)
    }
    #call the matrix from the list "get_as" from the cached
    data <- x$get_as()
    #solves the matrix
    xi <- solve(data, ...)
    x$set_asInverse(xi)
    xi
}