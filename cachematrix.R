## Put comments here that give an overall description of what your
## functions do

## Creating a special matrix below that can cache its inverse

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


## Following function calculates the inverse of the above special metrix. If inverse already exist then it will return the cached inverse

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("inverse already exist, getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
