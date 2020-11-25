## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Initialization
  m <- NULL
  
  ## set the matrix and the null in m
  set <- function(matrix) {
    x <<- matrix
    m <<- NULL
  }
  ## create the get function for the matrix, it returns the matrix when found
  get <- function() x
  
  ## set the inverse for the matrix
  setinverse <- function(inverse) m <<- inverse
  
  ##get the inverse of the matrix property if found or not
  getinverse <- function() m
  
  ##returns the list containing all the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## check the status of the inverse of m, if null it means it exists
  m <- x$getinverse()
  
  ## A message that says the inverse is in the cache
  if(!is.null(m)) {
    message("getting inverse of the matrix from cached data")
    return(m)
  }
  
  ## get the matrix
  matrix1 <- x$get()
  
  ## Calculate the inverse
  m <- solve(matrix1) %*% matrix1
  
  ## Set the inverse
  x$setinverse(m)
  
  ## Return the inverse
  m
}
