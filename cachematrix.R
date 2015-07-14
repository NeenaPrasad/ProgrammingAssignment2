##  Creates special matrix to cache the inverse
## it returns list that 

## sets value of matrix
## gets value of matrix
## sets the inverse of matrix
## gets the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        ## to set the matrix value
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## to get the value of matrix 
        get  <- function() x
        
        ## to store the value of inverse of the matrix
        setinverse <- function(solve) inverse <<- solve
        
        
        ## to get the stored value of x
        getinverse <- function() inverse
        
        ## returns a list
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the cached inverse of a matrix available
## if not available, inverse is computed, stored and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## reads the stored inverse of the matrix
        inverse <- x$getinverse()
        
        ## checks if the inverse is NULL, if not stored inverse
        ## is returned
        
        if (!is.null(inverse)) {
                
                ## message to notify when cached inverse is read  
                message("Retrieving cached data") 
                return(inverse)
        }
        
        ## inverse calculation is done if not stored, 
        
        data <- x$get()
        
        inverse <- solve(data,...)
        
        x$setinverse(inverse)
        
        ## inverse is returned
        inverse
}
