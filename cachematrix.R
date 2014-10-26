## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as an input
## to create a new matrix with the same elements
## of the input matrix while allowing for
## it's inverse to be store and retrived
## multiple times without computing the inverse again. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## This function computes the inverse of the
## special matrix if it hasn't already being computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()                     #get inverse
  if(!is.null(m)) {
    message("inverse from cached data")
    return(m) }
  data <- x$get()                     # read matrix elements
  m <- solve(data, ...)               # compute if inverse has not being computed
  x$setinv(m)                         # store inverse in the cache
  m
  
}
