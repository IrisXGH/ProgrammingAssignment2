## makeCacheMatrix creates a special "Matrix", 
## which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

# a<- makeCacheMatrix(matrix(c(5:8), nrow=2, ncol=2) )
# cacheSolve(a)

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) mi <<- inverse
  getInverse <- function() mi
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of the matrix created with the above function. 
 
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  ## first checks to see if the inverse has already been calculated. If so, 
  ## it gets the inverse from the cache and skips the computation. Otherwise, 
  ## it calculates the inverse of the data and sets the value of the inverse in the cache 
  ## via the setInverse function.
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setInverse(mi)
  mi ## Return a matrix that is the inverse of 'x'
}
