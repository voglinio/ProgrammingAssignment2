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
## Usage:
##
##  Create matrix 
##
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4
## 
##        x<-makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow=2, ncol=2))
##        
##        cacheSolve(x)
##              [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5
##        
##        cacheSolve(x)
##        Cached
##              [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5
##
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
  
  #
  # Stores the arguments in parnt evniroment
  set <- function(y){
    #
    # Checks if the existing matrix and the new one are identical.
    ## If not then update the contents of the matrix and its inverse
    if (!identical(x, y)){
      x    <<- y
      invX <<- NULL
    }
  }
  
  ##
  ## Get matrix 
  get <- function() {
    x
  }
  
  ##
  ## Get inverse matrix
  getinverse <- function() {
      invX
  }
  
  #
  # Set inverse matrix in parent environment
  setinverse <- function(inv) {
    invX <<- inv
  }
  
  #
  # A list of 4 setter/getter functions
  list (set=set, get=get, getinverse=getinverse, setinverse=setinverse)
}


## This function uses a `makeCacheMatrix` object x and returns
## its inverse. If the inverse already exists from previous cals
## makeCacheMatrix() returns it. If not it calculates the inverse
## using solve() and then uses x$setinverse() function to it back
## to the object.

cacheSolve <- function(x, ...) {
  ##
  ## Get the inverse from the argument
  invX <- x$getinverse()
  ##
  ## If the inerse is not null return it   
  if (!is.null(invX)){
    message("Cached")
    return (invX)
  }
  ##
  ## The inverse stored in the argument `x` is null
  ## So we use the matrix stored in `x`
  data <- x$get()
  ##
  ## To calculate the inverse using solve() 
  invX <- solve(data, ...)
  ##
  ## And set it back to the object `x`, so that next
  ## time cacheSolve is called on this object, the inverse
  ## matrix would be available.
  x$setinverse(invX)
  ##
  ## Return the inverse matrix 
  invX
}
