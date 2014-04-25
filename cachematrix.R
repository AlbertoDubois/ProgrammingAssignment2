# makeCacheMatrix creates a special matrix that can cache its inverse. cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.#

## This function takes an argument of type numeric matrix and returns a list with 4 list items (4 functions wrapped in a list)

makeCacheMatrix <- function(x = numeric()) {
  
  if(!is.matrix(x)) stop("x must be a matrix")
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse has been calculated and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)){        #Checks if it's in the cache
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()       
  m <- solve(data, ...)
  x$setinverse(m)
  
  m                      #returns the result
}
