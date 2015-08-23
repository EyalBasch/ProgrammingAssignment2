## R Programming Assignment 2
## The following pair of functions should be used to cache and/or calculate the inverse of a matrix 

    ## Please see below a suggested demonstration of the functions


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


## --Suggested usage of the functions--

    ## create a Matrix
        ## x<-matrix(c(4,3,3,2),2,2)

    ## Call Function makeCacheMatrix using Matrix x
        ## mf<-makeCacheMatrix(x)

    ## Show the Matrix
        ## mf$get()
        ##     [,1] [,2]
        ##[1,]    4    3
        ##[2,]    3    2

    ## Calculate the inverse Matrix using function cacheSolve
        ## cacheSolve(mf)
        ##     [,1] [,2]
        ##[1,]   -2    3
        ##[2,]    3   -4

    ## Show the cached inverse Matrix
        ## mf$getinvmatrix()
        ##     [,1] [,2]
        ##[1,]   -2    3
        ##[2,]    3   -4

    ## Use function cacheSolve the second time. This time the value will be retrieved from the cache.
        ##cacheSolve(mf)
        ##getting cached data
        ##     [,1] [,2]
        ##[1,]   -2    3
        ##[2,]    3   -4


