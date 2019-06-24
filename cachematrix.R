# The script is designed to cache the inverse of a matrix, which will prevent 
# recomputing the inverse.

# makeCacheMatrix creates a class for matrix, which contains 4 functions to 
# set and get the matrix, as well as set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
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

# cacheSolve function is designed to populate the cache. Specifically, if the 
# inverse are calculated before, then output the cache directly. Otherwise,
# compute the inverse using solve() function.

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

# test
x <- makeCacheMatrix(matrix(1:4,2,2))
x$get()
cacheSolve(x) # Calculate the inverse when computing for the 1st time
x$getinverse() # The inverse has been stored
cacheSolve(x) # Populate the cache directly
