## Functions that cache the inverse of a matrix

## Creating a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix(NA,dim(x)[1],dim(x)[2])
  set <- function(y) {
    x <<- y
    m <- matrix(NA,dim(x)[1],dim(x)[2])
  }
  get <- function() x
  setMatrix <- function(matrix) m <<- matrix
  getMatrix<- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## Returning an inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  if(sum(rowSums(m, na.rm = TRUE)) != 0) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
