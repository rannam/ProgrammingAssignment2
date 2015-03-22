## Below functions illustrate a method to cache the result and save on computation time.

## The first function makeCacheMatrix creates a matrix object that can cache its inverse.

## Input for this function is a square matrix and also invertible
## Output will be a list of functions to handle the caching of the result

makeCacheMatrix <- function(x = matrix()) {
        # Initialize the local variable
        inv <- NULL
        
        # To reset the parameters in the parent function
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }
        
        # Capture the value of input param
        get <- function() x
        
        # Set the value of inverse for given matrix in the parent function
        setinv <- function(inverse) inv <<- inverse
        
        # Get the inverse value
        getinv <- function() inv
        
        # function output
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## cacheSolve function actually calculates the inverse of the given matrix
## Returns a matrix that is the inverse of 'x'

## The input for this function will be the list of functions (output from above function)
## This input will have unique environment defined for the given matrix

cacheSolve <- function(x, ...) {
        
        # Capture the value of inverse from the required environment
        # If it is a new environment, the value will be null - and inverse will be calculated
        # If inverse value exists, then that value will be reutrned.
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Capture input matrix from above function (if above section is not returned)
        mat <- x$get()
        
        # Calculate the inverse
        inv <- solve(mat, ...)
        
        # Set the inverse value in the environment (caching/storing...)
        x$setinv(inv)
        
        # Return the value - inverse matrix of given matrix
        inv 
}
