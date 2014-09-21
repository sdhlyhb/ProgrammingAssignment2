##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        ## Set the value of the matrix;
		set <- function(matrix){
			x<<-matrix
			i<<- NULL
		
		}
        ## show the value of the matrix.
		get <- function()x
	## Set and get the inverse of the matrix;
		setinverse <- function(solve)i <<- solve
		getinverse <- function()i
		list( set= set, get= get,
			setinverse = setinverse,
			getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## get the inverse of the matrix
		i <- x$getinverse()
	## Check is if it is the matrix of which the inverse has already been calculated
		if(!is.null(i)){
			message("getting the cached inverse data")
			return(i)
		}
        ## if not, calculate its inverse.
		matrix <- x$get()
		i <- solve(matrix,...)
		x$setinverse(i)
		i

}
