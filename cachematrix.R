## \file cachematrix.R : This file implements methods to calculate the inverse
##                       of matrix. If the inverse has already been calculated
##                       and the matrix is unchanged, then the method outputs
##                       the cached matrix (inverse of the matrix)
## 
## \note: This is the solution implemented for the Programming assigment 2 and 
## demonstrates the various scoping rules.

## \function makeCacheMatrix : Creates a special vector of functions that 
## implement the foll - 
## 1. set the input matrix (also, reset the value of cached inverse)
## 2. get the input matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
    i <- NULL                               # Reset value of output matrix
    
    # Implement the set function: sets the input matrix
    set <- function(y) {                    
        x <<- y
        i <<- NULL
    }
    
    # Implement the get function: returns input matrix
    get <- function() x
    
    # Implement the setinv function: sets the inverse of the matrix
    setinv <- function(inv) i <<- inv

    # Implement the getinv function: returns inverse of the matrix
    getinv <- function() i
    
    # create the "special vector", which is the list of the above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## \function cacheSolve : Evaluates the inverse of the matrix "x"
## If the inverse of the matrix "x" has already been calculated
## then it simply returns the cache. Else, it calculates the inverse of the 
## input matrix. Sets the matrix to be cached and returns the calculated
## inverse.

cacheSolve <- function(x, ...) {

    i <- x$getinv()             # get local copy of output matrix (inverse)
    if(!is.null(i)) {           # if the matrix is not NULL, return cached output
        message("getting cached data")
        return(i)
    }
    
    # output matrix was NULL. So calculate the inverse
    data <- x$get()
    i <- solve(data, ...)
    
    # Cache the output matrix for future use.
    x$setinv(i)
    
    # return the inverse of the matrix.
    i
}
