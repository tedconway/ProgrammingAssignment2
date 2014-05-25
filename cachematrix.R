# Coursera - R Programming - Programming Assignment 2
# R functions to cache potentially time-consuming inverse matrix computations.

# Construct data structure from matrix passed by call. 
# Usage example: z <- makeCacheMatrix(matrix(c(4, 2, 7, 6), 2))

makeCacheMatrix <- function(x) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# Return inverse of matrix passed in data structure passed. Inverse is returned from cache,
# if available, or is computed using "solve" and saved in the cache for possible later use.
# Usage example: cacheSolve(z)

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
