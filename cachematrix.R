## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse

## 1. MakeCacheMatrix
## Create a list of four functions along with the matrix and inverse ojects in the environment
## The functions can be used to access/update the matrix/inverse

## 2. CacheSolve(x)
## Check if inverse of x is saved in the function environment of 'x'
## If saved -  Return the cached inverse of 'x'
## If not saved - Get data from 'x', compute inverse and save it to the cache in 'x'


#Creates a 'special matrix', which is matrix and inverse objects in the environment and a list of functions to access/update them
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## set Function to update the matrix and reset the matrix inverse
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## get function to retrieve the matrix
        get <- function() x
        
        ## setinv function to update the matrix inverse
        setinv <- function(inv) i <<- inv
        
        ## getinv function to retrieve the cached matrix inverse
        getinv <- function() i
        
        ## Return the list of four functions
        list(set = set, get = get,
             setinv= setinv,
             getinv= getinv)
}

## Returns a matrix that is the inverse of 'x'. Skips computation and returns cached inverse if present
cacheSolve <- function(x, ...) {

        i <- x$getinv()
        ## Check if inverse of x is saved in the function environment of 'x'
        if(!is.null(i)) {
                ## Inverse saved in 'x'. Return the cached inverse
                message("getting cached data")
                return(i)
        }

        ## Inverse not saved in 'x'. Get data from 'x', compute inverse and save it to the cache in 'x'
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
