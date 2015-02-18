## The first function takes a matrix and builds a "special matrix" (in fact a list)
##
## The second function takes a "special matrix" and checks if its inverse has already been 
## calculated. If it has, it returns its cached value
## If it hasn´t, it calculates the inverse and stores its value in the cache



## The following function takes a matrix and creates a list containing a function to 
## set and get (functions set and get) the values of the matrix, and to set and get 
## the values of its inverse (functions setinverse and getinverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function takes a special matrix (a list of 4 functions)
## and first checks if the inverse has already been calculated (via the function getinverse)
## if it has, it returns the cached value of the inverse
## if it hasn´t, it calculates it (via the function solve) and then stores it in the cache
## (via the function setinverse) and then returns the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
