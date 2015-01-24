## makeCacheMatrix creates a list of functions to be called by other function
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function will check the existance of a stored result 'm'
## If this result does not exist, calculates the inverse of matrix
## If the result exits, returned cached calculations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
