## Put comments here that give an overall description of what your

## Computation of matrix inversion is a costly and may benefit from caching
## the inverse, rather than compute it repeatedly. Two functions are included:
##
##     1. makeCacheMatrix to create the matrix
##     2. cacheSolve to return the inverse of the matrix, using the cached value
##              if it already exists.
##   Assumption:  The following code assumes that the matrix is invertible.




## Write a short comment describing this function
## makeCacheMatrix()
##       creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## Write a short comment describing this function
##  cacheSolve()
##      computes the inverse of the special "matrix" returned by the
##      makeCacheMatrix function. If the inverse has already been calculated (and
##      the matrix has not changed), then cacheSolve should retrieve the inverse
##      from the cache, rather than computing it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)
}
