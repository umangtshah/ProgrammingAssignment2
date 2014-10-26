## Caching the Inverse of a Matrix
## 'makeCacheMatrix' creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	## Initialize the inverse property
	i <- NULL

	## Method to set the matrix
	set <- function(y) {
        	x <<- y
            	i <<- NULL
    	}

    	## Method to get the matrix
    	get <- function() x
    		
    	
    	## Method to set the inverse of the matrix
    	setInverse <- function(inverse) 
        	i <<- inverse
    	
    	## Method to get the inverse of the matrix
    	getInverse <- function() i
        	
    	
    	## Return a list of the methods
    	list(set = set, get = get, setInverse = setInverse, 
	getInverse = getInverse)
}

## The following function returns the inverse of the matrix. It first
## checks if the inverse has already been computed. If so, it skips the
## computation and retrives the result. If not, inverse is computed and
## the value is set in the cache via 'setinverse' function.

cacheSolve <- function(x, ...) {

    	## Return a matrix that is the inverse of 'x'
    	i <- x$getInverse()

    	## Just return the inverse if its already set
    	if(!is.null(i)) {
            	message("getting cached data")
            	return(i)
    	}

    	## Get the matrix from our object
    	data <- x$get()

    	## Calculate the inverse using matrix multiplication
    	i <- solve(data)

    	## Set the inverse to the object
    	x$setInverse(i)

    	## Return the matrix
    	i
}
