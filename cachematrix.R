## Sample matrix
testmatrix <- matrix(data = sample(1:1000, 1000),nrow = 5,ncol = 5)

## To test that the code is working you can run the following commands in an R console (without the ' '):
## 1. 'testmatrix <- matrix(data = sample(1:1000, 1000),nrow = 5,ncol = 5)'
## 2. 'makeCacheMatrix(testmatrix)'
## 3. 'cacheSolve(makeCacheMatrix(testmatrix))'

## Or, you can simply run the entire command in one go using the following (without the ' '):
## 'cacheSolve(makeCacheMatrix(matrix(data = sample(1:1000, 1000), nrow= 5, ncol= 5)))'

## "Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse of a matrix." --Coursera

## "For this assignment, assume that the matrix supplied is always invertible." --Coursera

## "This function creates a special "matrix" object that can cache its inverse." --Coursera
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## "This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache." --Coursera

## "If X is a square invertible matrix, then solve(X) returns its inverse, so that is why we use solve()" --Coursera
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get() 
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
