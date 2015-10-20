## Programming Assignment 2
## Cache the Inverse of a Matrix

# makeCacheMatrix: a function which creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function (y) {     # set the matrix value
    x <<- y
    m <<- NULL
  }
  
  get <- function() x       # get the matrix value
  setinverse <- function(inverse) m <<- inverse    
  getinverse <- function() m  # get the inverse value
  
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve: a function that computes the inverse of the special "matrix" object returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
# not changed, then the cachesolve should retrieve the inverse from cache.)


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {  # if inverse is already cached
    message("getting cached data")   
    return(m)
  }
  
  data <- x$get()        
  m <- solve(data, ...)   # if not, calculating the inverse
  x$setinverse(m)  
  return(m)    
  
}