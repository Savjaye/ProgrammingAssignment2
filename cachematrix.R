
## This function returns a list of functions that a future function can utilize to cache the inverse of a function. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list (
    set = set, get = get,
    set_inv = set_inv,
    get_inv = get_inv)
}



## Caches the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if (!is.null(inv)){
    return(inv)
  }
  data <- x$get()
  # solve() returns the inverse of a matrix
  inv <- solve(data)
  x$set_inv(inv)
  inv
}
