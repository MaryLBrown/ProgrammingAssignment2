## Computation of a matrix inversion is costly and may benefit from caching,
## rather than computing it multiple times. The following code leverages lexical 
## scoping capabilities of R to address this. Two functions are included.
##      1. makeCacheMatrix accepts a matrix as input, stores the matrix and
##              returns a list containing 4 functions to set the matrix, 
##              retrieve the matrix, set the matrix inverse, retrieve the matrix
##              inverse. 
##              Note: makeCacheMatrix assumes that the supplied matrix is
##              invertible.
##      2. cacheSolve accepts the list returned by makeCacheMatrix and returns
##              the matrix inverse.  If the inverse has already been calculated,
##              cacheSolve retrieves it from the cache, rather than computing
##              it again. 
##  Usage:  Use makeCacheMatrix to instantiate the "special" matrix object. Use 
##              cacheSolve to retrieve the inverse.  


## makeCacheMatrix()
##    Create a special "matrix" object that can cache its inverse. 
##      
##       Pass in an an invertible matrix, x
##       Initialize the inverse as null.
##       Create functions to get and set the matrix and its inverse. 
##       The set functions store values in the cache, avoiding re-computation.
##       Return a the special "matrix", via a list of the get and set functions.
##       

makeCacheMatrix <- function(x = matrix()) {
        
        ## initalize the inv as null
        inv <- NULL
        
        ## set function for matrix
          set <- function(y) {                    
                x <<- y
                inv <<- NULL
          }
        
        ## get function for matrix
        get <- function() x  
        
        ## set function for the inverse, 
        ## uses the solve function to get the inverse
        setinv <- function(solve) inv <<- solve
        
        ## get function for the inverse
        getinv <- function() inv
        
        ## return list with functions
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}


##  cacheSolve()
##      computes the inverse of the special "matrix" returned by the
##      makeCacheMatrix function. If the inverse has already been calculated (and
##      the matrix has not changed), then cacheSolve should retrieve the inverse
##      from the cache, rather than computing it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        
        ## attempt to retrieve the inverse from x, by calling its getinv()
        inv <- x$getinv()
        
        ## if the value returned is not null, return the cached value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if the inverse was not set previously, get the matrix data from x,
        ## compute the inverse using the solve function, then store the inverse
        ## in the by calling the setinv function.
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        return(inv)
}
