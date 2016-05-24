## Caching the Inverse of a Matrix
## 

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
## Following is a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## A function to create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setInvMat <- function(inverse) invMat <<- inverse
        getInvMat <- function() invMat
        list(set = set, get = get, 
             setInvMat = setInvMat,
             getInvMat = getInvMat)
}


## A function to compute the inverse of the special "matrix" created by 
## makeCacheMatrix(). If the inverse has already been calculated (and the 
## matrix has not changed), then it returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInvMat()
        if(!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(data, ...)
        x$setInvMat(invMat)
        invMat
}
