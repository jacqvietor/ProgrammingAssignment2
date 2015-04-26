## Function makeCacheMatrix and cacheSolve work together to calculate and 
## cache the data of the inverse of a matrix.
## 
##
## Example calls: 
## > a <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2)).
## > cacheSolve(a) 

## Function makeCacheMatrix
## This function takes a matrix as input and defines the following 4 
## functions for it: set, get, setinv and getinv.
## 1) set: replaces the old matrix with a new matrix and sets the 
##    cached inverse to NULL
## 2) get: retrieves the (new) matrix data.
## 3) setinv: stores the inverse matrix in cache
## 4) getinv: retrieves the inverse matrix from cache


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function CacheSolve
## This function tests if the inverse matrix is already in cache
## (using the makeCacheMatrix function getinv). 
## If already in cache, it will retrieve and print 
## the inverse matrix and comment "getting cached data".
## If not stored in cache then:
## - the (new) matrix data are retieved 
##   (using the makeCacheMatrix get function)
## - the inverse of the matrix is calculated
## - the inverse matrix is stored in cache 
##   (using the makeCacheMatrix function setinv)
## - the inverse matrix is printed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
