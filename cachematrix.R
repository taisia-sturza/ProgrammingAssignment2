
makeCacheMatrix <- function(x = matrix()) {
	  ## function creates a special "matrix" object that can cache its inverse
	  ## I just modeled this function off of the makeVector function in the assignment description
	  ## instead of "mean" I used the function "solve" and made the special "vector" which is actually a list
	  m <- NULL
	  set <- function(y) {
		  x <<- y
		  m <<- NULL
        }
	  get <- function() x
	  setinverse <- function(solve) m <<- solve
	  getinverse <- function() m
	  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## I modeled this function off of the cachemean function in the assignment description
        ## I pulled the solve function here to get the inverse of the former function
        ## computes the inverse of the special "matrix" returned by makeCacheMatrix
	  ## if the inverse has already been calculated (and the matrix has not changed)
	  ## then cacheSolve should retrieve the inverse from the cache
	  ## can use the solve function in R 
	  ## if x is a square invertible matrix,
	  ## then solve(x) returns its inverse
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinverse()
	  if(!is.null(m)) {
		    	message("getting cached data")
			return(m)
	  }
	  data <- x$get()
	  m <- solve(data, ...)
	  x$setinverse(m)
	  m
}
