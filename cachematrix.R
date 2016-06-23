## Put comments here that give an overall description of what your
## functions do

## The "makeCasheMatrix" function creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse <<- inverse
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}

## The "cacheSolve" fuction computes the inverse of the special "matric"
## returned by "makeCacheMatrix" above. If the inverse has already been
## calculated, it gets the inverse from the cache. Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}


