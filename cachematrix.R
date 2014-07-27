## This exercise includes the functions "makeCacheMatrix" and "cacheSolve". 
##
## makeCacheMatrix creates a list of functions based on input invertible matrix x that will allow the user to:
## 1. set input invertible matrix x for manipulation
## 2. get/extract input invertible matrix x
## 3. set the value of the corresponding invertible matrix
## 4. get/extract the corresponding invertible matrix
##
## cacheSolve queries objects produced by makeCacheMatrix to either calculate 
## (if evaulating for the fist time), or extract from cache (if second or later evaluation) 
## inverse matrix solutions.



## makeCacheMatrix takes an invertible matrix as input. The function has thus-far not been made robust enough
## to handle exceptions nor does it check for valid inputs. For testing purposes, all input matrices 
## are therefore assumed to be invertible.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
  

}


## Calculates or extracts (from cache) the inverse of matrix, using list objects output by function makeCacheMatrix.

cacheSolve <- function(x, ...) {
        
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}