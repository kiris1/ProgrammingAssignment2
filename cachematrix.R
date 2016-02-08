#The first function, makeCacheMatrix creates a list containing a function to
#get the matrix
#set the inverse matrix
#get the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  setInverse <- function(InverseMatrix) inv <<- InverseMatrix
  getInverse <- function() inv
  list(get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#The following function calculates the inverse of the special matrix
#created with the above function. However, it first checks to see 
#if the inverse has already been calculated. If so, it gets the inverse matrix 
#from the cache and skips the computation. Otherwise, 
#it calculates the inverse matrix of the data and sets the inverse matrix 
#in the cache via the setInverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
