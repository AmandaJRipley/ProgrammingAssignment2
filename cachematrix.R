## Functions for getting the inverse of a matirix and saving computed values into a cache for faster retrieval in the future


## Creates a list containg a function to set and get the value of the vector, and set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that checks for cached data and returns if found, before executing the function for values not 
##  already present and saving them to the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
    if (!is.null(i)) {
       message("getting cached data")
       return(i)
    }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}