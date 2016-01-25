## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set the matrix using the passed argument
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the matrix setup using makeCacheMatrix function
  get <- function() x
  
  ## cache the inverser of the matrix using the argument inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ## get the inverse of the matrix
  getinverse <- function() inv
  
  ## return a list using the functions created above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Assumes a square matrix that is invertible
cacheSolve <- function(x, ...) {
  ## get the current value of the inverse 
  inv <- x$getinverse()
  
  ## if inverse is found not to be null then return the cached value
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## else calculate it 
  data<- x$get()
  
  ## solve function returns the inverse of an invertible matrix
  inv<-solve(data,...)
  
  ## set the inverse 
  x$setinverse(inv)
  
  ## return the inverse
  inv
}
