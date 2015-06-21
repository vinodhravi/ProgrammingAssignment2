## Put comments here that give an overall description of what your
## functions do
#The two functions are used to compute the inversion of a matrix that is given as input by the user, cache the inversion 
#matrix for future use to save computational resources.

## Write a short comment describing this function
#This function takes a matrix as an input and has getters and setters for that matrix and its inverse.
# This function assists the cacheSolve() function in caching.
makeCacheMatrix <- function(x = matrix()) {
invMatrix <- NULL
set <- function(y){	  #setter of input matrix
    x <<- y
    invMatrix <<- NULL
}
get <- function() x #getter of input matrix
setInvMatrix <- function(solveMatrix) invMatrix <<- solveMatrix #setter of inversion matrix
getInvMatrix <- function() invMatrix #getter of inversion matrix
list(set=set, get=get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix) 
}


## Write a short comment describing this function
#This function takes makeCacheMatrix function as an input and returns the inversion matrix as the output.
# The inversion matrix is returned from the cache if it is present otherwise it computes the inversion matrix, caches it and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInvMatrix()	#Get the inversion matrix from the cache
  if(!is.null(invMatrix)){		#Check if the inversion matrix from the cache is null or not
    message("getting cached data")
    return(invMatrix)	#Returning the cached inversion matrix
  }
  data <- x$get()	# Get the input matrix for which inverse is to be calculated
  invMatrix <- solve(data, ...) #Compute the inverse of the given matrix
  x$setInvMatrix(invMatrix)	#Cache the computed inversion matrix for future use
  invMatrix	#return the inversion matrix
}
