## These two functions create a 'cache' of the inverted matrix and 
## compute the inversion of any input matrix 


## This function creates a 'cache' copy of the matrix that is inputted
## It creates a list of functions to set/get the input matrix and the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  savecache <- function(inv) m <<- inv
  getcache <- function() m
  list(set = set, 
       get = get,
       savecache = savecache,
       getcache = getcache)
  
}


## This function computes the inversion of the matrix if the cache is empty

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getcache()
  if(!is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$savecache(m)
  m
}
