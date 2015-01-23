
#This function takes a square matrix and provides
#the wrapper for caching the matrixes inverse
makeCacheMatrix <- function(x = matrix()) {
    	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



# This function access returns the inverse of a matrix, 
#if an inverse insn't already cached it creates a cache of the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        			}
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}





