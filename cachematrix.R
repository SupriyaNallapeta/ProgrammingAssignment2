## makeCacheMatrix function creates a special "vector", which is really a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
    	x <<- y
    	inverse <<- NULL
  	}
  	get <- function() x
  	setInverse <- function(xInverse) inverse <<- xInverse
  	getInverse <- function() inverse
  	list(set = set, get = get,
    	setInverse = setInverse,
    	getInverse = getInverse)
}


## cacheSolve function checks whether the Inverse of Input matrix is already calculated. If it is calculated then it retrives the inverse matrix from the object and returns, otherwise calculated the inverse of a matrix, store it for future use and then returns

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
  	if(!is.null(inverse)) {
    	message("getting cached data")
    	return(inverse)
  	}
  	data <- x$get()
  	inverse <- solve(data, ...)
  	x$setInverse(inverse)
  	inverse
}
