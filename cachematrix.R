## These functions are usefull to compute the inverse of a matrix and save it in the cache. 

## This function makes a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function makes the inverse of the matrix defined before in the function makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
   message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
        ## It returns a matrix that is the inverse of 'x'
}


