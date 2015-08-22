## R Programming Assignment 2
## The following pair of functions should be used to cache and/or calculate the inverse of a matrix 

## The first function, makeCacheMatrix creates a special "Square Matrix (2x2)", which is really a list containing a function to
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the value of the inverse Matrix
## 4. get the value of the inverse Matrix

makeCacheMatrix <- function(x = matrix()) { m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
     }
    get <- function() x
    setinvmatrix <- function(solve) m <<- solve
    getinvmatrix <- function() m
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## However, If the inverse has already been calculated, then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinvmatrix()
    if(!is.null(m)) {
       message("getting cached data")
       return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvmatrix(m)
    m
}
