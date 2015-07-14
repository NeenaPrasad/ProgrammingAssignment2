##  Creates special matrix to cache the inverse
## it returns list that 

## sets value of matrix
## gets value of matrix
## sets the inverse of matrix
## gets the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get  <- function() x
        
        setinverse <- function(solve) inverse <<- solve
        
        getinverse <- function() inverse
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the cached inverse of a matrix available
## if not available, inverse is computed and stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
        
        if (!is.null(inverse)) {
                message("Retrieving cached data")
                return(inverse)
        }
        
        data <- x$get()
        
        inverse <- solve(data,...)
        
        x$setinverse(inverse)
        
        inverse
}
