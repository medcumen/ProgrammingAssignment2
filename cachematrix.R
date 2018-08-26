## CLW 2018-08-26
## Get the inverse of a matrix and cache it OR
## Retrieve the inverse if it already exists in the cache

## 1st function: Get inverse from cache if it already exists
##               Otherwise, create the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Return a matrix that is the inverse of 'x'
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 2nd function: Set the inverse of the matrix

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
