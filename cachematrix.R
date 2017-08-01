## Function receives a matrix to calculate it inverse
## and then stores it to be used in the second function

## Function takes a matrix given as parameters for the function
## then calculates the inverse of it

makeCacheMatrix <- function(x = matrix()) {
  var_i <- NULL
  set <- function(y) {
    x <<- y
    Var_i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Looks for the inverse of the matrix to see if stored in cache
## if it is not stored, then it calculates the inverse
## and returns it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
