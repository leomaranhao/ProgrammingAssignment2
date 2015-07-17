## This is the solution of the Programming Assignment 2 of the R Programming 
## course at Coursera.

## Author: Léo Maranhão de Mello
## Date: 2015-07-17

## We will create 2 functions:
##
## 1) makeCacheMatrix: This function receives an invertible matrix and gives 
##              back a list of 4 functions that allow you to to set and access
##              the values of the matrix and its inverse. 
##              This list represents a special type of matrix. One with cached 
##              values of the matrix itself and its inverse.
##              The values of the matrix and its inverse will be stored in the 
##              cm.m and cm.i variables.
##
## 2) cacheSolve: This function receives the list created by makeCacheMatrix and 
##              returns the inverse of the matrix represented by that list.
##              If the inverse matrix has already been cached, the cached value 
##              will be returned. Otherwise the inverse of the matrix will be 
##              calculated, cached and returned.



## This function receives an invertible matrix and gives back a 
## list of 4 functions that allow you to to set and access
## the values of the matrix and it's inverse.

makeCacheMatrix <- function(cm.m = matrix()) {
        cm.i <- NULL
        set <- function(y) {
                cm.m <<- y
                cm.i <<- NULL
        }
        get <- function() cm.m
        setinv <- function(inverse) cm.i <<- inverse
        getinv <- function() cm.i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function receives the list created by makeCacheMatrix and 
## returns the inverse of the matrix represented by that list.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
