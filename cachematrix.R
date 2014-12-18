## R Programming Cource 
## Prorgamming Assignement 2
## Costas Voglis
##
## In this assignement we are asked to create a wrapper structure
## around a matrix object that will store its inverse and persist
## it until we change the matrix. We will follow the implementation
## of the example code makeVector.
## We will provide setters and getters for the matrix data and
## a cached call to its ivnerse. 

##
## Function makeCacheMatrix(x) takes as input a matrix x and
## create a list of objects that describe the wrapper object 
##
## Initialy the inverse is set to NULL
## The wrapper provides the following setters/getters:
## For internal storage x and invX (matrix and inverse) the 
## parent environment is used (<<-)
##
## set(x)          : Takes a matrix as input and initializes the internal
##                   storage. If the argument is not identical to the I=internal matrix 
##                   then it is assigned as the matrix and the inverse is initialized to NULL  
## get()           : Returns the matrix
## setinverse(invX): Set the inverse matrix equals to the argument (used from cacheSolve())
## getinverse()    : Returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  
  set <- function(y){
    if (!identical(x, y)){
      x    <<- y
      invX <<- NULL
    }
  }
  
  get <- function() {
    x
  }
  
  getinverse <- function() {
      invX
  }
  
  setinverse <- function(inv) {
    invX <<- inv
  }
  
  list (set=set, get=get, getinverse=getinverse, setinverse=setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invX <- x$getinverse()
  if (!is.null(invX)){
    message("Cached")
    return (invX)
  }
  data <- x$get()
  invX <- solve(data, ...)
  x$setinverse(invX)
  invX
}
