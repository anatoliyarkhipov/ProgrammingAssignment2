## Pair of functions which allow you reduce calculations
## by caching the result of a matrix inversion.


## Create a "special" matrix that can contains an original 
## matrix data and its inversion. The get/set functions used 
## for get and set original data, and the getSolve/setSolve 
## used for get and set inversion of this data

makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL

    set <- function(y) {
    	x <<- y
        s <<- NULL
    }
    get <- function() x

    setSolve <- function(solve) s <<- solve
    getSolve <- function() s

    list(
    	set = set, 
    	get = get,
        setSolve = setSolve,
        getSolve = getSolve
    )
}


## Get cached inversion and return it. If the invesion is 
## NULL, then calculate it and save in the "x" before return 

cacheSolve <- function(x, ...) {

    s <- x$getSolve()

    if (is.null(s)) {
    	message('Calculate inversion of the matrix')
        s <- solve( x$get() )
    	x$setSolve(s)
    }

    s
}
