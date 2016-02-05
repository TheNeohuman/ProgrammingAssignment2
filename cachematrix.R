## Put comments here that give an overall description of what your
## functions do

## This is my function to create the Special Matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## Sets the new value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Gets the stored value of the matrix
  get <- function() x
  ## Sets the value of the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  ## Retrieves the stored value of the inverse of the matrix
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This is my function to retrieve or calculate the inverse of the Special Matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Gets the stored value of the inverse of the matrix
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## If the stored value is NULL then it calculates the inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
