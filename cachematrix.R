## The two functions cache the inverse of a matrix 
## rather than computing it repeatedly

## The function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

## The function cacheSolve returns a matrix that is the inverse of 'x'.
## If the inverse is already cached, it uses the cached data. 

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
