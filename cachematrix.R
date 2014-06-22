## This pair of functions solves and inverse matrix and stores it in a
## created cache. When later users want to use the function to solve inverse
## matrices, the function first inspects the cache to see if the matrix has 
## already been solved and stored. If it has, then the function pulls forward the
## solved matrix rather than recalculating the inverse from scratch.

## This function creates the cache in which the matrix is stored.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function returns the inverse of a matrix after first checking the cache
## to see if the inverse has already been stored. If the result is already in the
## cache, it returns the cached result. If it is not in the cache, then it returns
## the inverse and stores it in the cache for future retrieval.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
