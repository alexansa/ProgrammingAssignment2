## This pair of functions caches the inverse of a matrix.

## The function makeCacheMatrix convert a matrix to a matrix object that support caching its inverse

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

## The function cacheSolve calculates the inverse of the special matrix returned by makeCacheMatrix function above.
## If the inverse has already been calculated, then the cacheSolve retrieves the cached inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        else { 
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
        }
}
