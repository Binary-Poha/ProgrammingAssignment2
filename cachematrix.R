## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setMean <- function(solveMatrix) inv <<- solveMatrix
  getMean <- function() inv
  list(set = set, get = get,setMean =setMean, getMean = getMean)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getMean()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv  
}

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
