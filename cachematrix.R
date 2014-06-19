## The overall functions will cache an inverse of a matrix 
## and whenever user has to calculate the inverse, the function will 
## get the inverse from the cache. This can save time to calculate. 
## If there is nothing in the cache, the function will solve for the inverse. 

## This function is a list of four functions:
## 1) set the value of the specific matrix
## 2) get the value of the matrix
## 3) set the inverse matrix of the stored matrix
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL                               # set inverse matrix as null initially
  set <- function(y) {                          # sets a matrix as designated - y
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x                           # returns the stored matrix x
  setinverse <- function(inv) inv_mat <<- inv   # sets the inverse matrix as designated - inv
  getinverse <- function() inv_mat              # returns the inverse matrix inv_mat
  list(set = set, get = get,                    # returns the list of the four functions
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function will get the inverse matrix from the cache,
## and if the cache has no inverse, it will calculate the inverse.


cacheSolve <- function(x, ...) {
  inv_mat <- x$getinverse()                     # stores inverse matrix from makeCacheMatrix in inv_mat
  if(!is.null(inv_mat)) {                       # if the cached inverse is not null, return cached inverse
    message("getting cached data")
    return(inv_mat)
  }
  mat <- x$get()                                # if not, store the matrix in concern from makeCachematrix
  inv_mat <- solve(mat)                         # solve for inverse matrix and store it in inv_mat
  x$setinverse(inv_mat)                         # set the inverse matrix of x as inv_mat
  inv_mat
  
  ## Return a matrix that is the inverse of 'x'
}