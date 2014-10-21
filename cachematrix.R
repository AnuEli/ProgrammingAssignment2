
## Functions makeCacheMatrix and cacheSolve compute the inverse of given matrix. Functions use 
## "<<-" operator to assing computed inverse to cache (different environment from current environment. 
## If the inverse is already in the cache, it can be returned from there and time-demanding 
## computation can be skipped.


## Function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function( x = matrix()) {
  inv <-NULL
  set <- function( y ){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function( inverse ) inv <<- inverse
  getInverse <- function() inv
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function returns a matrix which is inverse of 'x'. If the inverse is already calculated,
## function returns the inverse from cache and skip the computation.

cacheSolve <- function(x, ...) {
  inv <-x$getInverse()
  if ( !is.null( inv )){
      message(" getting cached data")
      return( inv )    # returns inverse from cache
  }
  data <-x$get()
  inv <- solve( data,... )  # counts the inverse of a square matrix
  x$setInverse( inv ) # stores inverse to cache
  inv
}
