## makeCacheMatrix prepares a vector of functions to set and cache inverse of
## the provided matrix. It also sets the matrix in the vector

## cacheSolve function will either determine (and cache) the inverse of matrix
## in the provided vector. In the future, the cached inverse shall be provided
## whenever the function is used

## makeCacheMatrix returns a vector of functions that set and get the passed
## matrix. Additionally, functions (set) to return Inverse of provided matrix 
## Finally, a function to get the inverse from cache is provided in the vector

makeCacheMatrix <- function(theMatrix = matrix()) {
    
    ## intitialize inverse as NULL for future checks
    theInverse <- NULL
    
    ## the set function shall set the provided matrix in the vector
    ## & initialize the inverse whenever it is called
    setMatrix <- function(y){
        theMatrix <<- y
        theInverse <<- NULL
    }
    
    ## the get function shall get the cached matrix
    getMatrix <- function() theMatrix
    
    ## the set function caches the provided inverse Matrix
    setInverse <- function(z) theInverse <<- z
    
    ## the get function returns cached inverse matrix (initially set to NULL)
    getInverse <- function() theInverse
    
    ## return the vector containing the functions
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Provide inverse of supplied matrix if it is the same
## 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Check if inverted matrix is already available (not NULL)
    theInverse <- x$getInverse()
    if(!is.null(theInverse)){
        ## inverse matrix exists, inform on using cache
        message("Using cache value, skipping calculation")
        return(theInverse)
        ## no more work!
    }
    
    ## need to determine inverse and store in cache
    
    ## get the matrix in vector
    workMatrix <- x$getMatrix()
    
    ## determine inverse Matrix
    theInverse <- solve(workMatrix,...)
    
    ## cache the inverse Matrix before completing the function
    x$setInverse(theInverse)
    
    ## provide the inverse Matrix as return from function
    theInverse
}
