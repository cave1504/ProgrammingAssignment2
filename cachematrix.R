## cachematrix.R
##
## This file contains two functions:
## 1. makeCacheMatrix: creates a special "matrix" 
##                     object that can cache its inverse 
## 2. cacheSolve: returns the inverse of the "matrix"


## makeCacheMatrix creates a special "matrix" object with 
## functions to get and set the value of the matrix and 
## to get and set the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }

        get <- function() x
        setinverse <- function(y) x_inv <<- y
        getinverse <- function() x_inv
        
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve produces the inverse of the matrix returned 
## by makeCacheMatrix. The inverse is retrieved 
## from cache if it has been calculated before. 
## Otherwise it is calculated using the 'solve' function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        x_inv <- x$getinverse()
        if (!is.null(x_inv))  {
              message("getting cached data")
              return(x_inv)
        }

        x_inv <- solve(x$get())
        x$setinverse(x_inv)
        x_inv
        
}
