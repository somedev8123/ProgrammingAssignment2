## makeCacheMatrix: Creates a matrix with cacheable inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse with NULL
  inverse <- NULL
  ## set: assign mat to x and NULL to inverse
  set <- function(mat) {
    x <<- mat
    inverse <<- NULL
  }
  ## get: return x
  get <- function() x
  ## setinverse/getinverse: used to set and get
  ## cached inverse matrix, accordingly.

  ## setsolve: Set the inverse
  setsolve <- function(inv) inverse <<- inv
  
  ## getsolve: Return the inverse
  getsolve <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## getsolve from matrix x
  inverse <- x$getsolve()

  ## If cache is found, return it
  if(!is.null(inverse)) {
    message("getting cache data") ## side-effect consistent with example
    return(inverse)  ## FUNCTION EXITPOINT
  }

  ## If we make it this far, obviously no cache was found
  ## compute the inverse and assign to cache
 
  data <- x$get()
  inverse <- solve(data, ...)
  x$setsolve(inverse)
 
  ## Return it to the caller
  inverse
}
