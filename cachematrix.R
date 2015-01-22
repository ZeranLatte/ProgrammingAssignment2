## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and return a list of functions
## When the new matrix is read, the inverse value is set to NULL
## Wed Jan 21 21:48:37 2015

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(n) inverse <<- n
      getInverse <- function() inverse
      list(set=set, get=get, setInverse = setInverse,
           getInverse = getInverse)

}


## This function takes makeCacheMatrix return as parameter and
## check if the inverse matrix is cached; if not it sets the inverse in the object 
## and stores the inverse in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      check <- inverse == solve(x$get())
      if(is.null(inverse)) {
            dat <- x$get()
            result <- solve(dat)
            x$setInverse(result)
            return(result)
      }
      x$getInverse()
}
