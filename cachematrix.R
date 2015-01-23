## This program calucates the inverse of a matrix with the use of cache value.

## This function takes a matrix input
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function()x
	setinv <- function(solve)inv
	getinv <- function()inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function returns the inverse value. If the inverse value has been calculated, 
## it will give the cache value instead of recalcuating the inverse.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
}
