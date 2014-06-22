## Functions below allow creation of a matrix which can cache its inverse. 
## If the inverse is not cached or matrix changes, a fresh inverse is 
## computed and stored into the cache.

## makeCacheMatrix - Creates a matrix which allows to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) invs <<- inverse
  getinv <- function() invs
  list(set=set, get=get, 
       setinv=setinv, 
       getinv=getinv)
}


## cacheSolve - If the matrix object does not have a inverse Computed 
## and cached the inverse is computed, cached into the object and 
## returned as a return value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invs <- x$getinv()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinv(invs)
  invs
}
