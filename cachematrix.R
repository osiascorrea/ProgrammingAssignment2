# makeCacheMatrix function creates a special "matrix"
# containing a function to set the value of the matrix,
# get the value of the matrix. set the inverse matrix of the matrix,
# get the inverse matrix of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

# cacheSolve calculates the inverse matrix of the special "matrix"
# checks to see if the inverse has already been calculated and if there is skips it.
# else, it calculates the inverse of the 'data' and sets the inverse in the cache 
# with setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
