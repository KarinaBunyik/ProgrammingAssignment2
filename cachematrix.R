## The following functions implement a cached version
## of matrix inverse calculation using the built in R 
## matrix type and the solve function

## Extended matrix type. Enables cached matrix inverse caclulation.
## Only for invertable square matrices
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Extended solve function. Implements cached matrix inverse calculation
## using the makeCacheMatrix type. Only for invertable square matrices
cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if (!is.null(inverse)) {
                message("getting the cached inverse of matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}
