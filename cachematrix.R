## Functions below allow creation of a matrix which can cache its inverse. 
## If the inverse is not cached or matrix changes, a fresh inverse is 
## computed and stored into the cache.

## Creates a matrix which allows to cache its inverse.
## @param mat input matrix
makeCacheMatrix <- function(mat = matrix()) {
  invs <- NULL
  
  ## function to update the stored matrix to new value
  set <- function(newmat) {
    mat <<- newmat
    ## reset inverse if matrix changes.
    invs <<- NULL
  }

  ## function to return the stored matrix
  get <- function() mat
  
  ## function to set the inverse of the matrix for caching.
  setinv <- function(inverse) invs <<- inverse
  
  ## function to return the cached value of matrix, NULL if not set
  getinv <- function() invs

  list(set=set, get=get, 
       setinv=setinv, 
       getinv=getinv)
}


## cacheSolve - If the matrix object does not have a inverse Computed 
## and cached the inverse is computed, cached into the object and 
## returned as a return value.
## 
## @param mat cacheable matrix, capable of storing the inverse.

cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'x'
  invs <- mat$getinv()
  ## if a cached inverse exists return it.
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  
  ## get the matrix data anc compute the inverse.
  data <- mat$get()
  invs <- solve(data, ...)
  
  ## cache the newly computed inverse in the matrix object.
  mat$setinv(invs)
  invs
}
