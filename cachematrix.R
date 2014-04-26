## Perform inversion of a matrix, cache the result, and assert the cache did
## work by expecting a message in console ("getting cached data")
##
## Then verify the inversion was computed properly by multiplying the inverted
## matrix by the original matrix. Expecting an identity matrix

## ------------------------------------------------------------
## Build a list of functions to perform the cached matrix
makeCacheMatrix <- function(x = matrix()) {
  # init cached result to null
  m <- NULL
  
  # functions
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setMatrixInverted <- function(matrixInverted) m <<- matrixInverted
  getMatrixInverted <- function() m
  
  # list of functions
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setMatrixInverted = setMatrixInverted, 
       getMatrixInverted = getMatrixInverted)
}

## ------------------------------------------------------------
## Search the cache or resolve and store if the result is not in cache
cacheSolve <- function(x, ...) {
  m <- x$getMatrixInverted()
  if(!is.null(m)) {
    message("getting data From Cache")
    return(m)
  }
  message("Data not found in the cache . computing ...")
  
  data <- x$getMatrix()
  m <- inverse(data, ...)
  x$setMatrixInverted(m)
  m
}

## ------------------------------------------------------------
## Doing the actual matrix inversion
inverse <- function(x){
  solve(x) # ginv or solve
}

## Apply calculation
v <- cbind(c(0.5, 0.3), c(0.4, 0.9))
cv <- makeCacheMatrix(v)
im <- cacheSolve(cv)

## Verify results
im <- cacheSolve(cv)
print(im %*% v)


