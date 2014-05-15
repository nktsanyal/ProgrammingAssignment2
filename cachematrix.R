## a pair of functions that cache the inverse of a matrix.

## steps to follow to verify successful execution of the script

## load the script: source("cachematrix.R");
## initialize: a <- makeCacheMatrix();       
## define a square matrix: x <- rbind(c(1, -1/4), c(-1/4, 1));
## set the vector: a$set(x);
## get the vector: a$get();
## calculate the inverse: cacheSolve(a);
## when is called back use the cached inverse: cacheSolve(a);                        # 


## makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

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