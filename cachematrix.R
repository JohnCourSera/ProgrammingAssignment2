##
## Write an R function is able to cache potentially time-consuming computations 
##
## Create a pair of functions that cache and compute the inverse of a matrix.
##
## The first function "makeCacheMatrix", creates a special "matrix", 
## which is really a list containing a function to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse
##
## This Second function "cacheSolve" computes the inverse of the special "matrix"
## returned by the first function "makeCacheMatrix".
## If the inverse has already been calculated (and the matrix has not changed), 
## then retrieve the inverse from the cache rather than recomputed.
############################################################################### 

## This function creates a special "matrix" object to cache the value of the inverse.
makeCacheMatrix <- function(mx = matrix()) {
        ## initial the inverse
        inversemx <- NULL
        
        ## set the value of the matrix
        set <- function(x) {
                mx <<- x;
                inversemx <<- NULL;
        }
        
        ## get the value of the matrix
        get <- function() return(mx);
        
        ## set the value of the inverse
        setinverse <- function(inv) inversemx <<- inv;
        
        ## get the value of the inverse
        getinverse <- function() return(inversemx);
        
        ## creates a list containing all the above 4 function
        return(list(set = set, get = get, set_inverse = setinverse, get_inverse = getinverse))
}

## This function computes the inverse of the special "matrix" returned by "makeCacheMatrix" 
## If the inverse has already been calculated (and the matrix has not changed), 
## then retrieve the inverse from the cache rather than recomputed.

cacheSolve <- function(mx, ...) {
        ## try to retrieve the value of the inverse from cache
        inversemx <- mx$getinverse()
        
        ## if found the inverse, just cache the value rather than recomputed.
        if(!is.null(inversemx)) {
                message("Retrieve from cached data.")
                return(inversemx)
        }
        
        ## if the inverse not in the cached data, compute the inverse
        data <- mx$get()              ## get the value of the matrix
        invsersemx <- solve(data, ...)  ## computes the inverse
        mx$setinverse(inversemx)        ## set the value of the inverse into cache
        return(inversemx)
        
}