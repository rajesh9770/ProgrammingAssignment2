## Functions to computes inverse of an matrix
## 
## makeCacheMatrix implements caching to cache the inverse of an given matrix.
## It provides a wrapper around the given matrix that caches the inverse of the matrix 
## and then returns it when needed

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    ## if inverse is already found in cache, return it.
    if (!is.null(inv)){
        return (inv)
    }
    ## not in the cache,  find the inverse and store it in the cache 
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
