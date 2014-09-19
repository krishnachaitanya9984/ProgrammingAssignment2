## Put comments here that give an overall description of what your
## functions do

##Make a square matrix using matrix function
## squarematrix <- matrix(data = c(10,20,30,40), nrow = 2,
##       ncol = 2,byrow = FALSE,dimnames = NULL)

## cache your inverse of the squarematrix using makeCacheMatrix function
## cacheableMatrix = makeCacheMatrix(squarematrix)
## call cachesolve to calculate the inverse
## inv1 = cacheSolve(cacheableMatrix)
## print to see it
## inv1
## call cachesolve to calculate the inverse.This time it will return the cache one without computing again
## inv2 = cacheSolve(cacheableMatrix)
## print to see it
## inv2


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  set <- function (newx) {
    x <<- newx
    cachedinverse <<- NULL
  }
  get <- function() x
  setcachedinverse <- function(inverse) cachedinverse <<- inverse
  getcachedinverse <- function() cachedinverse
  
  list(set = set, 
       get = get, 
       setcachedinverse = setcachedinverse,
       getcachedinverse = getcachedinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
  cached <- x$getcachedinverse()
  if (! is.null(cached)) {
    return(cached)
  }
  m <- x$get()
  cached <- solve(m)
  x$setcachedinverse(cached)
  cached
}
