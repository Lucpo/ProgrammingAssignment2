## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL
  set <- function( matrix ) 
  {
    m <<- matrix
    i <<- NULL
  }
  
  get <- function() m
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  m <- x$getInv()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInv(m)
  
  m
}
