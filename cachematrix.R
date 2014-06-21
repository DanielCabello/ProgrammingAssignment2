## The functions allow caching the values of a matrix and its inverse matrix in a special object
## These stored values can be recovered instead of being computerized again.

## FUNCTION 1: makeCacheMatrix
## It generates a cacheable object that includes a list with four functions

makeCacheMatrix <- function(x = matrix()) {  
        m <- NULL
    
    # 1st Function - set the values of the matrix
    set <- function(y) {                     
        x <<- y
        m <<- NULL
    }
    
    # 2nd Function - get the values of the stored matrix
    get <- function() x                      
    
    # 3rd Function - set the values of the inverse matrix
    setsolve <- function(solve) m <<- solve
    
    # 4th Function - get the values of the stored inverse matrix
    getsolve <- function() m                 
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## FUNCTION 2: cacheSolve()
## Function that calculates the inverse of the special "matrix" created
## and cached with the function 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
 
 # if the inverse matrix is cached the function retrieves their values
 # and displays a message
 m <- x$getsolve()
 if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
  
  # If the inverse matrix is not cached the function calculates the 
  # inverse of the matrix and caches 
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    
  # shows the inverse matrix on screen
    m
}
