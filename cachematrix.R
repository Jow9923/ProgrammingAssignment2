## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##CREATE A SPECIAL MATRIX OBJECT THAT CAN CACHE ITS INVERSE

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ##set i to NULL
  set <- function(y) { ##set is a function(y)
    x <<- y ## <<- allows storage of x value in parent, not global environment
    i <<- NULL ## <<- stores i as NULL in parent, not global environment
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv) ##This saves the variables we will use later as a list
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## compute the inverse of the matrix returned by  'make cache matrix'. If the
## inverse has already been calculated, cachesolve should retrieve the inverse
## from the cache.

  cacheSolve <- function(x, ...) { #solve matrix saved using makeCacheMatrix
    i <- x$getinv() 
    if (!is.null(i)) { ##if i is NOT null and we already have something in cache
      message("getting cached from makeCacheMatrix")
      return(i) ## show us what we have in getinv
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
  }
