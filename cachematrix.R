## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function( m = matrix() ) {
  
  ## initialize invers
  i <- NULL
  
  ## set matris method
  set <- function( matrix ) 
  {
    m <<- matrix
    i <<- NULL
  }
  
  ## get matrix method
  get <- function() m
  
  ## set inverse matrix
  setInv <- function(inverse) i <<- inverse
  
  ## get inverse matrix
  getInv <- function() i
  
  ##return method list
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## return then invers (m) of matrix (x)
  m <- x$getInv()
  
  ## just return already set inverse
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## get matrix
  data <- x$get()
  
  ## calculate inverse
  m <- solve(data) %*% data
  
  ## set inverse
  x$setInv(m)
  
  ## return matrix
  m
}
