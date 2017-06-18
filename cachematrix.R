## This function creastes a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  Get <- function()x
  SetInverse <- function(SolveMatrix)I <<- SolveMatrix
  GetInverse <- function()I
  list(Set = Set, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$GetInverse()
  if(!is.null(I)){
    message("Getting Cached Data")
    return(I)
  }
  data <- x$get()
  I <- solve(data,...)
  x$SetInverse(I)
  I
}
