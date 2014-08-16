## Function makeCacheMatrix takes as imput a matrix, and creates a list of functions that will be useful
## for caching and retreiving the matrix and its inverse.

makeCacheMatrix <- function(X = matrix()) {
	m <- NULL

	## Function "set" caches matrix X
	set <- function(y) {
		X <<- y
		m <<- NULL
	}

	## Function "get" allows retrieval of X
	get <- function() X

	## Function "setinverse" caches the inverse of X
	setinverse <- function(solve) m <<- solve

	## Function "getinverse" allows retreival of the content of X$setinverse
	getinverse <- function() m

	## The output is a list of those 4 functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Function "cacheSolve" takes as input a cached matrix, and returns the inverse of the corresponding matrix.
## If the inverse is already cached, the function reads is from the cached object and returns it.
## If the inverse in not already cached, the function calculates it and caches it before returning it.
## Let's assume for more clarity that Xcached is the cached version of a matrix called X

cacheSolve <- function(Xcached, ...) { 	        ## where Xcached is the cached version of the matrix of interest, has been created using "makeCacheMatrix"
	m <- Xcached$getinverse()		## looks for the inverse of X in the "getinverse" object, the place where the inverse would be cached if it has been computed before.
	if(!is.null(m)) {			## if the inverse has been calculated before, it was cached into Xcached$getinverse, and is now loaded in m. We have our result without further calculation
		message("getting cached data")
		return(m)			## return the inverse matrix
	}
	data <- Xcached$get()			## if m is empty, we need to calculate the inverse of X. We start by loading the value of X into a variable "data"
	m <- solve(data, ...)			## we compute the inverse of X using the "solve" function
	Xcached$setinverse(m)			## we write the value of the inverse in Xcached
	m					## return the inverse matrix
}
