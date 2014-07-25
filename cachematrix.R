## This is assginment 2 under R programming course. In this assignment we learned 
## how to cache a object by <<- operator. The purpose of the cache is to record
## time-consuming operation in cache. Like inverse a matrix for a large matrix, it
## takes too long to compute the inverse. If the value of inverse of the matrix cachhed
## when we need it again, it can be looked up in the cache rather than recomputed. 
## in this assignment you also utilize scoping rules of R to preserve state inside of 
## R object.
## Below are two functions that used to create a special object that stores a matrix 
## and caches its inverse. 

## in this makeCacheMatrix: it creates a special object, which is a list containing
## a function to:
## 1. set the value of of matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <-function() {x}
  
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() {inverse}
  list(set = set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function return a matrix that is the inverse of 'x'
## It first checks to see if the inverse has been caculated. if so, it gets the inverse 
## from the cache and skips caculation. otherwise, it caculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setInverse function. 
## 

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <-solve(data,...)
  x$setInverse(inverse)
  inverse
}
