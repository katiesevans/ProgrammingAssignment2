## These functions work together to solve the inverse of the matrix and cache the output
## to avoid repeating the same calculations

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function will calculate the inverse of a matrix. If it has already been calculated before, it will used cached data.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv      
}

# Testing the functions
m <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
cacheSolve(m)
cacheSolve(m)


