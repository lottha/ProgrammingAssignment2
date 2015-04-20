## Functions to invert a matrix and hold the result in 
## cache for use in subsequent function calls

## This function creates a vector of functions to set the value of a matrix, 
## get the matrix, set the value of the inverse matrix and get the value of 
## the inverse matrix
makeCacheMatrix <- function(mat = matrix()) {
	invMat <- NULL
	set <- function(y){
		mat <<- y
		invmat <<- NULL
	}
	get <- function() mat
	setinv <- function(inv) invMat <<- inv
	getinv <- function() invMat
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function calculates the inverse of a matrix supplied as a 
## makeCacheMatrix function vector. If the inverse has already been 
## calculated, the inversion is skipped and the cached value returned 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invMat <- x$getinv()
	if (!is.null(invMat)){
		message("getting cached data")
		return(invMat)   
	}
	mat <- x$get()
	invMat <- solve(mat)
	x$setinv(invMat)
	invMat		
}
