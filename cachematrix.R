## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversa) inv <<- inversa
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  if(!is.null(x$get()) && is.matrix(x$get())){
    squared <- dim(x$get())[1] == dim(x$get())[2]
    if(squared == T){
      matrix <- x$get()
      inv <- solve(matrix)
      x$setinv(inv)
    }
    else
      message("matrix must be sqare")
    
  }
  else
    message("This is not a matrix!")
  inv
}
