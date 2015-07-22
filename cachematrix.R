## Function for Caching the Inverse of a Matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
        m <- matrix(NA,nrow(x),ncol(x))
        set <- function(y) {
                x <<- y
                m <<- matrix(NA,nrow(x),ncol(x))
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(all(!is.na(m))) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m  ## Returns a matrix that is the inverse of 'x'
}