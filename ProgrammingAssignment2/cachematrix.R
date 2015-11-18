## Caching the Inverse of a Matrix
##
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. Below are two functions that are used to creat a special object that
## stores a matrix abd caches its inverse matrix.

## Function 1 --- makeCacheMatrix()
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Input:  a square invertible matrix x
## Output: a special list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse matrix
##              4. get the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
                        x <<- y
                        inv <<- NULL
    }
    get = function() x
    setinv = function(solve) inv <<- solve 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Function 2 --- cacheSolve() 
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
## Input:  the special "matrix" returned by makeCacheMatrix above
## Output: a inverse matrix of the original square invertible matrix x

cacheSolve <- function(x, ...) {

    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv 
}


### Testing Scripts 
# > source("C:/Users/Dehui/Documents/datascience/ProgrammingAssignment2/cachematrix.R")
# > x<-matrix(c(1,2,3,4),nrow=2,ncol=2)
# > solve(x)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# > xx<-makeCacheMatrix(x)
# 
# > cacheSolve(xx)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 

