## This function takes a matrix as an argument and returns 
## a list,l, of functions:
##     l$get returns the original matrix
##     l$set sets the matrix to be a new matrix
##     l$getinverse returns the matrix inverse
##     l$setinverse sets the inverse to the argument passed to it

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a list passed to it that is created
## by a call to makeCacheMatrix and returns the inverse
## of the matrix as well as creating a cached copy.
## subsequent calls to cacheSolve with the same list object
## result in returning the cached inverse of the matrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x' and cache its 
## value
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}