##Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makecachematrix <- function(x = matrix()) {
  rng <- NULL
  set <- function(y) {
    x <<- y
    rng <<- NULL
  }
  get <- function() x
  setinv <- function(inverse)rng <<- inverse
  getinv <- function(){ 
    inver<-ginv(x)
    inver%*%x
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  rng <- x$getinv()
  if(!is.null(rng)) {
    message("getting cached data")
    return(rng)
  }
  data <- x$get()
  rng <- solve(data, ...)
  x$setinv(rng)
  rng
}
