# The function calculate the inverse of the matrix
## and save it to the cache,
## the next time we calculate the matrix inverse, 
## the saved value is used and not recalculated again

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}



## It checks if the inverse was calculated already
## If yes it gets the inverse from the cache
## If not it makes the calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

