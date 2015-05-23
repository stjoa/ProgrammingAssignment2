## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# create the special matrix
# with i for cached inverse value
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # getter setter matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {
    x
  }
  
  # getter setter inverse cache value
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## Write a short comment describing this function
# cached version of solve function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  # if inverse is already calculated  (i != NULL) return cached value
  if(!is.null(i)) {
    message("inverse value cached ... returning")
    return(i)
  }
  # else solve matrix, store value in i and return inverse
  else {
    data <- x$get()
    i <- solve(data)
    x$setsolve(i)
    return(i)
  }
}
