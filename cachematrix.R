## R Programming: Programming Assignment 2: Lexical Scoping
##
## Function 'makeCacheMatrix' is used to store and display
## a matrix and its inverse. Function 'cacheSolve' checks
## if inverse of matrix exists, if not, then calculates and
## stores it.

## Store and display matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	mat <- NULL
	set <- function(y) {
		x <<- y
		mat <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) mat <<- solve
	getsolve <- function() mat
	list(set = set, get = get,
	   setsolve = setsolve,
	   getsolve = getsolve)
}


## Check if inverse of a matrix exists in the cache. If it 
## doesn't then calculate it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	mat <- x$getsolve()
	if(!is.null(mat)) {
		message("Retrieving cached inverse")
		return(mat)
	}
	data <- x$get()
	mat <- solve(data,...)
	x$setsolve(mat)
	mat
}
