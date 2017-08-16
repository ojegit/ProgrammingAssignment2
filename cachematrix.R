## R Programming: Programming Assignment 2: Lexical Scoping
##
## Function 'makeCacheMatrix' generates a list and functions
## capable adding matrices and removing them from
## the list. 
##  
## Function 'cacheSolve' checks if the list generated with 
## 'makeCacheMatrix' contains inverse of a matrix. If the 
## inverse does not exist then it is calculated and added
## to the list.

## Create a list which contains capabilities for 
## setting and getting values of matrix and its inverse

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


## Check if inverse of a matrix exists in the cache generated
## with 'makeCacheMatrix'. If it doesn't, then calculate 
## inverse and put it in the cache.

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
