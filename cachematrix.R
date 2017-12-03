## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function (y){
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  setInv <- function(newInv){
    matInv <<- newInv
  }
  getInv <- function(){
    matInv
  }
  list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        if(!is.null(Inv)){
          message("Inverse already calculated !!")
          return(Inv)
        }
        m <- x$get()
        Inv <- solve(m, ...)
        x$setInv(Inv)
        Inv
}
