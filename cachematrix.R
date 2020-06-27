#Following functions are able to cache the inverse of a given matrix when the matrix remains
# constant and are useful when the inverse needs to be calculated multiple times. The input matrix
# should always be an invertible one.


#This function takes input as matrix and returns 4 functions to set and get matrix and to cachethe inverse of a matrix and get the same

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y #cache the matrix throught <<- opertaor(replaces the matrix value in the enclosing environment)
    inverse <<- NULL
  }
  get <- function() x 
  setinverse <- function(invc) inverse <<- invc # Cache the inverse of matrix calculated from the cachesolve matrix for the first time. "<<-" here replaces the "inverse" value when the matrix is same  
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#function to calculate inverse of matrix and return inverse(cached)

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse() #returns "inverse" value if its cached(if the matrix is same) else returns NULL
  if(!is.null(inverse)) {
    message("getting cached data") #to let the user know if the value returned is from cache or not
    return(inverse) # if the value is cached the functions stops here by returning the Cache value
  }
  data <- x$get() # get the matrix when the matrix/inverse is not cached
  inverse <- solve(data, ...) # calculate inverse of inout matrix using solve funstion
  x$setinverse(inverse) #caches the inverse value of input matrix
  inverse
}
