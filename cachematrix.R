        #This function CacheMatrix 
        #creates a set of functions

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y = matrix()) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## CacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
