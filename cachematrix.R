## These functions create a matrix-like object which will cache its inverse

## This function creates a special "matrix", which is really a list
## containing a fcuntion to set the matrix, get the matrix, set the inverse
## and get the inverse

makeCacheMatrix <- function(matrix = matrix()) {
    inv <- NULL
    set <- function(m) {
      matrix <<- m
      inv<<- NULL
    }
    get <- function() matrix
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created
## using makeCacheMatrix but it first checks to see if the inverse has
## already been computed and, if so, skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached inverse")
            return (i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
