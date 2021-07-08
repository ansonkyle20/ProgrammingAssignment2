## The script consists of two functions, makeCacheMatrix, and cacheSolve.
## makeCacheMatrix, the first function in the script, consists of set, get, setInverse, and getInverse.
## This function also introduces an operator, "<<-".


makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x<<- y
		i<<- NULL
		}
	get <- function() {x}
	setInverse <-function(inverse) {i <<- inverse}
	getInverse <- function() {i}
	list(set = set, get = get, 
	     setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates for the inverse of the matrix, or returns an already cached solution for the inverse.

cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if(!is.null(i)) {
		message("retrieving cached data")
		return(i)
	}
	mat <-x$get()
	i <-solve(mat, ...)
	x$setInverse(i)
	i

        ## The last part of the function returns the inverse.
}
