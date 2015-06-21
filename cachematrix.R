## the main idea of this code is that in some times it makes sence to keep 
## a matrix and its inverse matrix together. So when you need an inverse 
## you do not need to calculate it each time
## looks like some king of class in OO approach.

## This makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse. 
## makeCacheMatrix is a list of functions to work with the inverse of a matrix 
## contains following methods:
## set -- to write a matrix in the enviroment (to set it)
## get-- to get a matrix that was previously set(to read it)
## inverset -- to write an inverse matrix
## inverget -- to read an inverse matrix
## calculating of an inverse matrix is made in a separate function!!!

makeCacheMatrix <- function(x = matrix()) {
i <- NULL ## inverse for a new matrix has not been calculated yet
        set <- function(y) {
                x <<- y
                i <<- NULL  
        }
        get <- function() x
        inverset <- function(s) i <<- s ##we do not calculate we only writing!!
        inverget <- function() i

## makeCacheMatrix returns a list containing 4 functions

        list(set = set, get = get, 
             inverset = inverset,
             inverget = inverget)
}




## cacheSolve --This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## cacheSolve needs a class created with makeCacheMatrix and not asimple matrix!!
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
i <- x$inverget()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
## else -- inverse for this matrix was not calculated before, so
## we have to do it now and to keep i for the future 
        data <- x$get()
        i <- solve(data, ...)
        x$inverset(i)
        i
}

