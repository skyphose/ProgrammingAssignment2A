## miguel hsu
## skyphose.com
## coursera week 3 assignment 2

## make the matrix and the inversion then save to cache
makeCacheMatrix <- function(x = matrix()) {
  ## declare variables
  c = NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  ## how to do the inverting
  setInverse <- function(inverse) c <<- inverse
  getInverse <- function() c
  ## spit out our functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## solve the cache and output
cacheSolve <- function(x, ...) {
  c <- x$getInverse()
  ## check if the matrix was already done
  if (!is.null(c)) {
    message("Getting Cached Data")
    return(c)
  }
  ## calcualte and output!!!
  data <- x$get()
  c <- solve(data,...)
  x$setInverse(c)
  ## output!
  c
}
