##The first function takes a matrix and get a list and the second
## takes this list a cache the matrix

## This function takes a matrix and return a list with neccesary objects
## to cache the matrix

makeCacheMatrix <- function(x = matrix()) {
  ma <- NULL
  set <- function(y) {
    x <<- y
    ma <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) ma <<- matrix
  getmatrix <- function() ma
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This function takes a makeCacheMatrix object and return the inverse of the matrix
## fastly with cache

cacheSolve <- function(x, ...) {
  ma <- x$getmatrix()
  if(!is.null(ma)) {
    message("getting cached data")
    return(ma)
  }
  data <- x$get()
  ma <- solve(data , ...)
  x$setmatrix(ma)
  ma
  ## Return a matrix that is the inverse of 'x'
}
