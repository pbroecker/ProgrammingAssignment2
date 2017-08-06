## These functions are used to cache the inverse of a matrix, in doing so, it will not have to be computed over and over

## makeCacheMatrix creates a matrix object and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve computes the inverse of the matrix returned by the makeCacheMatrix funtion

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


## Return a matrix that is the inverse of 'x'

x <- matrix(c(1:4),2,2)
x1 <- makeCacheMatrix(x)
cacheSolve(x1)
