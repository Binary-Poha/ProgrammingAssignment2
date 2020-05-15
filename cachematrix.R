## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setMean <- function(solveMatrix) 
  inv <<- solveMatrix
  getMean <- function() inv
  list(set = set, get = get,setMean =setMean, getMean = getMean)

}

## This function computes the inverse of the special "matrix" created by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inv <- x$getMean()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
}

