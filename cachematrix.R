## makeCacheMatrix creates a special matrix object
## it first checks to see if the the inverse of the matrix has already been calculated
## If so, it gets the matrix inverse from the cache and skips the computation
## Otherwise, it calculates the matrix inverse of the data and sets the value of the matrix inverse in the cache via the setinverse function
makeCacheMatrix<- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



cacheSolve  <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
}
        ## Return a matrix that is the inverse of 'x'

