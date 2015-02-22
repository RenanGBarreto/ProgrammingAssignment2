## This file have 2 functions: makeCacheMatrix and cacheSolve
## The functions will work as a cache for the function solve() 
## (function that return the inverse of a matrix). 
## The cache will increase the speed when doing the inverse of lots of matrixes
## that, time to time, repeat.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## FUNCTION: makeCacheMatrix
## DESCRIPTION: Creates a cache matrix that will save aditional data
## (cache) when used by the function 'cacheSolve'.
## PARAM: x A matrix
## RETURN: the CacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(sol) m <<- sol
  getsolve <- function() m 
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## FUNCTION: cacheSolve
## DESCRIPTION: Calculates the inverse of a matrix of a matrix 
## created with makeCacheMatrix function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverse in the cache via the setsolve
## function.
## PARAM: x A cachematrix created with the function 'makeCacheMatrix'
## RETURN: a matrix that is the inverse of 'x'
cacheSolve <- function(x) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("Getting cached data on function 'cacheSolve'")
    return(m) ## cache hit
  }
  data <- x$get()
  m <- solve(data)  ## cache miss
  x$setsolve(m)
  m
}
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## TESTS: (PLEASE IGNORE THOSE)
## matrix_a <- makeCacheMatrix(matrix(c(4,3,3,2),nrow=2,ncol=2)); #Create the cached matrix
## print("Original Matrix A: ");
## print(matrix_a$get());
## matrix_a_inversed <- cacheSolve(matrix_a); #Calculate the inverse
## print("Inverted A Matrix: ");
## print(matrix_a_inversed);
## matrix_a_inversed_2 <- cacheSolve(matrix_a); #Calculate the inverse AGAIN!
## print(matrix_a_inversed_2);
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 