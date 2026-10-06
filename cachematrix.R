## These two functions cache the inverse of a matrix, 
## so the potentially costly inversion is done only once per matrix.
## makeCacheMatrix wraps and stores the matrix and its inverse.
## cacheSolve returns the inverse, computing it with solve() only if it is not already cached.

## makeCacheMatrix: creates a special matrix object, 
## i.e. a list of four functions to set/get the matrix and set/get its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: returns the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse is already cached it is retrieved from the cache. 
## Otherwise it is computed with solve(), stored in the cache, and returned.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
