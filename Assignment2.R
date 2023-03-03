#------------------------------------------------------------------------
# an implementation of the makeCacheMatrix and cacheSolve functions:
#------------------------------------------------------------------------

# Create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Compute the inverse of a cached matrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

#--------------------------------------------------------------
# an example of using the functions:
#----------------------------------------------------------------
# Create a test matrix
mat <- matrix(c(1, 2, 3, 4), nrow = 2)

# Create a cached matrix object
cacheMat <- makeCacheMatrix(mat)

# Compute the inverse and cache it
cacheSolve(cacheMat)
# Output: 
#          [,1]     [,2]
# [1,] -2.00000  1.50000
# [2,]  1.00000 -0.50000

# Retrieve the cached inverse
cacheSolve(cacheMat)
# Output: getting cached data
#          [,1]     [,2]
# [1,] -2.00000  1.50000
# [2,]  1.00000 -0.50000

# Change the matrix and invalidate the cache
cacheMat$set(matrix(c(1, 0, 0, 1), nrow = 2))
cacheSolve(cacheMat)
# Output:
#          [,1] [,2]
# [1,]       1    0
# [2,]       0    1

