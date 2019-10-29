## First function returnes a list of 4 functions
## 

## Set and get are basically writing and reading data in the cache
## same with the setinv and getinv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y= matrix()){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list( set = set, get = get, setinv = setinv , getinv = getinv)
}


## cachesolve checks the cache for the previously calculated inverse.
## and returnes the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<- solve(data,...)
  x$setinv(inv)
  inv
    ## Return a matrix that is the inverse of 'x'
