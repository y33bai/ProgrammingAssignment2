## This function in overall can firstly cache the calculated inverse of a matrix, and 
## retrieve those cached value if needed.

## makeCacheMatrix function creates a matrix that primarily used to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function firstly checks whether the inverse already been stored or not
## If so then it pulls out the cached data
## If not then it will compute the inverse of a input matrix, cache it and output it

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m  ## Return a matrix that is the inverse of 'x'
}
