makeCacheMatrix <- function(x = matrix()) {
        
        ## creates a square invertible matrix
        ## and returns a list containing function to
        ## set the matrix
        ## get the matrix
        ## set the inverse
        ## get the inverse
        
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinvrs <- function(inverse) invrs <<- inverse
        getinvrs <- function() invrs
        list(set <- set, get <- get, setinvrs <- setinvrs, 
             getinvrs <- getinvrs)
}

cacheSolve <- function(x, ...) {
        
        ## This is the output of makeCacheMatrix()
        ## It returns the inverse of the matrix input to 
        ## makeCacheMatrix().
        ## In other words, it returns a matrix that is the 
        ## inverse of 'x'
        
        invrs <- x$getinvrs()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinvrs(invrs)
        return(invrs)
}
