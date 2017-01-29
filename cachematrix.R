## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## SOLUTION FUNCTION 1:
## This function mimics the given makeVector() function and attaches
## four functions to the target matrix: 
## setting and geting the values of the matrix and doing the same
## for the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## Solution Function 2
## Like the cacheMean function, this function gives the inverse
## of a matrix, but checks first whether this has already been
## done (in which case it initially returns a "getting cached data").
## If it hasn't been done, this function deposits an inverse to be
## stored in the environment or cache (I guess).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
           inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
