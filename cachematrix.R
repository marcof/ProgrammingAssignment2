## a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        ## creates a special "matrix", which is really a list containing a function to:
        m <- NULL
        
        ## 1.set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## 2.get the value of the matrix
        get <- function() x
        
        ## 3.set the value of the inverse matrix
        setinverse <- function(inverse) m <<- inverse
        
        ## 4.get the value of the inverse matrix
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        
        ## calculates the inverse of the special "matrix" created with the above 
        
        ## It first checks to see if the inverse has already been calculated
        m <- x$getinverse()
        
        ## If so, it gets the inverse from the cache and skips the computation
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Otherwise, it calculates the inverse of the data and sets the value 
        ## of the inverse in the cache via the setinverse function.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
