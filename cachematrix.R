## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Here we take advantage of the scoping rules of the R language to
## preserve state inside of an R object, creating two functions to create and
## use a special "matrix" object that caches the matrix's inverse.

## Here is example usage for the two functions, from a terminal:
## > source("cachematrix.R")
## > m <- matrix(1:4, 2, 2)
## > m
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cm <- makeCacheMatrix(m)
## > cacheSolve(cm)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(cm)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## Create a special "matrix" which is actually a list containing several
## functions for getting and setting the value of the matrix and its inverse.

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


## Return a matrix that is the inverse of 'x', using the cached value if it
## exists. If it does not exist, calculate the inverse of the matrix and cache
## the result before returning.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
