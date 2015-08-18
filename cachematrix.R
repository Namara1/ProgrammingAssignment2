## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

## This special "matrix" is a list containing a function to 
## 1.set the value of the matrix (set)
## 2.get the value of the matrix (get)
## 3.set the inverse of the matrix (setinverse)
## 4.get the value of the inverse (getinverse)


makeCacheMatrix<-function(x=matrix()){
  m <- NULL  ## m is the inverse matrix
  
  ## whenever the matrix is changed with "set", its inverse is set to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inversematrix) m <<- inversematrix
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## cacheSolve function computes the inverse of the special "matrix" x returned by
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  ## we check that the inverse matrix has already been calculated (is not NULL).
  ## in that case, it is simply returned with a message
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if the inverse matrix has not yet been calculated, the solve function computes it
  ## it is stored into the "m" variable.
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
