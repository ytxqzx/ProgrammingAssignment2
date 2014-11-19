## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of 
## a matrix rather than computing it repeatedly
## This pair of functions is used to cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object, 
## which is a list of functions.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      set <- function(y){
            x <<- y
            I <<- NULL
      }
      get <- function() x
      setInv <- function(Inv) I <<- Inv
      getInv <- function() I
      list(set = set, get = get, 
           setInv = setInv, getInv = getInv)
      
      

}


## This function computes the inverse of the special "matrix" 
## returned by "makeCacheMatrix" above.

cacheSolve <- function(x, ...) {
      I <- x$getInv()
      if(!is.null(I)){
            messeage("getting cached data")
            return(I)
      }
      data <- x$get()
      I <- solve(data,...)
      x$setInv(I)
      I
      
        ## Return a matrix that is the inverse of 'x'
}
