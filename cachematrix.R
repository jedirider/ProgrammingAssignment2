## As the assignment states, these functions are useful for caching the
## inverse of a matrix since matrix inversion is so costly computationally.

## The first function creates a matrix object that will store a given matrix
## and cache the inverse of the matrix. This function uses the 'solve'
## function of R to inverse the matrix. The other steps mirror the 'vector' example
## given in the assignment.

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL     
        set <- function(y) { 
                x <<- y     
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
## This function completes the makeCacheMatrix function by either retrieving
## the inverse matrix from cache, or computing a new one.

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
