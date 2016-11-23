## Cache matrix for inverse matrix calculation
## Functions:
## makeCacheMatrix: This function creates a special "matrix" object 
##   that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
##   returned by makeCacheMatrix above. If the inverse has already been 
##   calculated (and the matrix has not changed), then the cachesolve 
##   should retrieve the inverse from the cache

## Initialize cache inverse matrix and functions

makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse matrix cache
    inverse_matrix  <- NULL
    # define functions: set, get, setinverse, getinverse
    set = function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get = function() x
    setinverse = function(inv_matrix) inverse_matrix <<- inv_matrix
    getinverse = function() inverse_matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Initialize cache inverse matrix and functions

cacheSolve <- function(x, ...) {
    # retrieve the cached inverse matrix
    inv_matrix = x$getinverse()
    
    # if the inverse matrix existed in the cache, use it
    # otherwise calculate the inverse and store in cache
    if (!is.null(inv_matrix)){
        message("getting cached data")
    } else {
        message("calculating inverse, loading to cache")
        inv_matrix  <- x$setinverse(solve(x$get(),...))
    }
    return(inv_matrix)
}

## Test function

testCacheMatrix <- function() {
    set.seed(1110201)
    mat1=matrix(rnorm(10000),100,100)
    temp_matrix = makeCacheMatrix(mat1)
    # first time the cache should be empty, the inverse should be calculated
    cacheSolve(temp_matrix)
    # second time, the cache should be used
    cacheSolve(temp_matrix)
    # third time, the cache should be used
    cacheSolve(temp_matrix)
    return("Test completed")
}
