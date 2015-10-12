## The makeCacheMatrix() function creates a special vector, 
## which is really a list containing a function to 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve() function takes a matrix as input and returns its inverse. 
## Computation of inverse happens only if inverse if not in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## Compute inverse
  if (nrow(data) != ncol(data)){
    stop("Given matrix is not a square matrix")
  }
  if (det(data) == 0){
    stop("Matrix is singular. Inverse does not exist")
  }
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
