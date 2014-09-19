## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Returns a list that contains four functions
## These functions are exposed via a list to the following
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function(){
    x
  }
  setinverse = function(inv){
    m <<- inv
  }
  getinverse = function(){
    m
  }
  list(set = set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## Write a short comment describing this function
## Takes in a makeCacheMatrix type 
## Returns the cached inverse if it exists
## If not, it calculates the inverse and caches the value in makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mtx <- x$get()
  m <- solve(mtx)
  x$setinverse(m)
  m
}


## Used the following for testing
a <- matrix(c(4,3,3,2), nrow=2)
b <- makeCacheMatrix(a)
# this should return null
b$getinverse()
cacheSolve(b)
# this should return 
# [-2   3]
# [ 3 -4]
b$getinverse()
