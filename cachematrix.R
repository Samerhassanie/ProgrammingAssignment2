## The purpose of these functions is to compute the inverse of a matrix
## In addition it stores the inverse to save time if the inverse is needed
## in the future

## makeCacheMatrix returns a list from which the inverse of a matrix can
## be cached when needed

makeCacheMatrix <- function(M = matrix()) {
  invM <- NULL
  set <- function(y) {
    M <<- y
    invM <<- NULL
  }
  
  get <- function() M
  
  setInv <- function(solve) invM <<- solve
  getInv <- function() invM
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve computes the inverse of a matrix if it hasn't been calculated yet
## otherwise it returns the saved invers matrix

cacheSolve <- function(M, ...) {
  invM <- M$getInv()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  mat <- M$get()
  invM <- solve(mat)
  M$setInv(invM)
  invM
}

