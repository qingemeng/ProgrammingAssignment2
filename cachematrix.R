########################################################################################################
## Second programming assignment for R Programming course                                             ##
## This script contains two R function that is able to cache potentially time-consuming computations. ##
## 26, April, 2015                                                                                    ##
## by Gemeng Qin                                                                                      ##
########################################################################################################

# This function creates a special "matrix" object that can cache its inverse.
# It's a list of functions containing get and set functions for matrix and its inversed matrix.
# Args:
#   x: empty matrix
# Return:
#   A list of functions: set, get, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL
    set <- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverseValue)
    {
        inverse <<- inverseValue
    }
    
    getInverse <- function() inverse
    
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
#
# Args:
#   x: input matrix
# Return:
#   inverse: inversed matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    
    # If matrix inverse is cached
    if(!is.null(inverse))
    {
        message("getting cached matrix inverse")
        return(inverse)
    }
    
    # Calculate matrix inverse if it's not cached
    # and cache the matrix inverse, then return
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    return(inverse)
}

