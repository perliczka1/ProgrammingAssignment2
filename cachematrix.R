## Functions can be used to create vector-like object 
## in order to calculate and then store its inverse

## Function creates all necessary functions and
## sets up the environment to store matrix x and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  
  list(set = set, get = get,
  setInv = setInv,
  getInv = getInv)
  
}

## Function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  xMatrix = x$get()
  i = solve(xMatrix, ...)
  x$setInv(i)
  i
}
