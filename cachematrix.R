

makeCacheMatrix <- function(x = matrix()) {
    data <- x
    cache <- NULL
    setMatrix <- function(y) {
        data <<- y
        cache <<- NULL  # Reset the cache when the matrix changes
    }
    getMatrix <- function() {
        data
    }
    cacheInverse <- function(x, ...) {
        if (!is.null(cache)) {
            # Return the cached inverse if it exists
            message("Cached data is retrieved.")
            return(cache)
        }
        
        # Compute the inverse
        message("Inverse is calculated...")
        inv <- solve(data)
        
        # Cache the inverse
        cache <<- inv
        
        # Return the computed inverse
        inv
    }
    
    # List of functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse)
}

# Computes the inverse of the "matrix" that we get by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
    data <- x$getMatrix()
    res <- x$cacheInverse()
    res
}
