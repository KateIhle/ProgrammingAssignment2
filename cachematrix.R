## makeCacheMatrix and cacheSolve are functions that work together to create
## a matrix object, calculate the inverse of the matrix, and store that inverse
## to be recalled later. makeCacheMatrix creates the matrix and stores the inverse, while cacheSolve

## makeCacheMatrix is a function that creates a matrix object that can then cache its inverse for later recall. 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL						# variable m is assigned the value NULL
		set <- function(y) {				#sets the value of the matrix
				x <<- y				#assigns the value of x, allowing it to be found outside of the function
				m <<- NULL			#allows the variable m to be reassigned a value from the cacheSolve function
		}
		get <-function() x				#gets the value of the matrix
		setinverse <- function(solve) m <<-solve  	#sets the value of the inverse
		getinverse <- function() m		        #gets the value of the inverse
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                                                                # creates a list of the componenets of makeCacheMatrix															
}


## This function returns a matrix that is the inverse of the matrix created by makeCacheMatrix. It will calculate the inverse of x, but will first check for astored inverse matrix within makeCacheMatrix.

cacheSolve <- function(x, ...) {
		m <- x$getinverse()				#links the value of m in cacheSolve to the stored value in makeCacheVector
			if(!is.null(m)) {			#if there is a value of m stored in makeCacheVector:
				message("getting cached data")	# then prints the message
				return(m)			#and returns the stored inverse matrix
			}
			data <- x$get()				#If there is no stored inverse, gets the original matrix 
			m <- solve(data, ...)			#calculates the inverse matrix
			x$setinverse(m)				#sets the inverse matrix value to be cached in makeCacheVector
## Return a matrix that is the inverse of 'x'
}
