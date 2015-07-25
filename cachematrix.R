

#The makeCacheMatrix function creates a special "matrix", which is really a list containing a function to 
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the inverse of the matrix
#4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse = matrix())m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#The cacheSolve function computes the inverse of the special "matrix" created with the makeCacheMatrix function. 
#It first checks to see if the inverse has already been computed. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it computes the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
