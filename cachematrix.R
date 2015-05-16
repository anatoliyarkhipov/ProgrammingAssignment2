## Pair of functions which allow you reduce computations
## by caching the result of a matrix inversion.
## 
## Usage example:
## 
## > x = matrix(1:4, 2, 2)
## > x_cache = makeCacheMatrix(x)
## 
## ## At first call the "'Calculate inversion of the matrix'" message will be displayed
## > cacheSolve(x_cache) 
## Calculate inversion of the matrix
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## ## At any other call the "'Calculate inversion of the matrix'" will not be displayed
## ## because the cached inversion will be returned without new calculation
## 
## > cacheSolve(x_cache) 
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## ## You can set an new matrix to the cache object. 
## ## In this case inversion will be calculated once again.
##
## > x_cache$set( matrix(17:20, 2, 2) )
##
## > cacheSolve(x_cache)
## Calculate inversion of the matrix
##      [,1] [,2]
## [1,]  -10  9.5
## [2,]    9 -8.5
##
## > cacheSolve(x_cache)
##      [,1] [,2]
## [1,]  -10  9.5
## [2,]    9 -8.5



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
        s <- solve( x$get(), ... )
    	x$setSolve(s)
    }

    s
}
