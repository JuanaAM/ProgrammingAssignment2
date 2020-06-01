## This function cache the inverse of a square invertible matrix

## makeCacheMatrix takes an square invertible matrix as an argument and
## return an object with four functions: set, get, setinverse, getinverse
## and the objects x and inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve takes an object makeCacheMatrix as argument, check whether the 
## inverse is already chached, if so it retrives it; otherwise, it computes the 
## the inverse and cache it using the function setinverse()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
