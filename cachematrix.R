## cachematrix.R
## makeCacheMatrix creates a "cacheMatrix" object that enhances a matrix by
## storing previously calcualted matrix inverses in a cache. Inverses of "chacheMatrix"
## objects are calculated via the cacheSolve function.

## creates a "cacheMatrix" object that stores a matrix 'x' and its inverse in the variable 
## cache_inv. Includes setter and getter functions for the stored matrix  and the
## cached inverse
makeCacheMatrix <- function(x = matrix()) {
	cache_inv <- NULL # object to store cached matrices
  
    # set incoming matrix y to internal matrix x
    # in the makeCacheMatrix environment
	set_matrix <- function(y) {
	    x <<- y
	    cache_inv <<- NULL # remove existing cached inverse 
	}
  
    # return the current matrix
	get_matrix <- function() x
  
    # put a calculated inverse in the cache
	setinv <- function(inv) cache_inv <<- inv
  
    # get cached inverse
	getinv <- function() cache_inv
  
    # list to access functions
	list(set_matrix = set_matrix, 
         get_matrix = get_matrix,
	     setinv = setinv,
	     getinv = getinv)
}


## Given a cacheMatrix objects created by the function makeCacheMatrix, this
## function checks to see a matrix inverse is already calculated by its NULL status.
## Returns the cached inverse if stored and calcuates it via the Solve function otherwise. 
cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
       
       # get the current cached inverse
       cache_inv <- x$getinv()
       
       # check already calculated, i.e., not NULL
       if(!is.null(cache_inv)) {
           message("getting cached inverse")
           return(cache_inv)
       }
       
       X <- x$get_matrix() # get data matrix X
       
       # check if square
       if (dim(M)[1] == dim(M)[2]) {
           # matrix square
           cache_inv <- solve(X, ...) # solve inverse 
       } else {
           print("Matrix is not square and not invertable")
           return
       }
       
       x$setinv(cache_inv) # store calculated inverse for later
       
       cache_inv # return
}
