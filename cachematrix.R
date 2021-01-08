##  Following the example, the function creates a special 'vector' which is really 
## a list containing a function to set and get the vector's value as well as 
## set and get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<-NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) invmatrix <<- inverse
  getinv <- function() invmatrix
  list(set = set, get=get,
       setinv = setinv, getinv = getinv)
}

## Following the example, this function calculates the inverse of the special 'vector'
## created with the above function.
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean 
## in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  invmatrix <- x$getinv()
  if(!is.null(invmatrix)){
    message("getting cached data")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setinv(invmatrix)
  invmatrix
        ## Return a matrix that is the inverse of 'x'
}
