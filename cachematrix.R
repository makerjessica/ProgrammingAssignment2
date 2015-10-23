##Programming Assignment 2- R Programming
##
##Assignment requires the creation of 2 functions.
##The first function, "MakeCacheMatrix" is a function of Matrix X.

makeCacheMatrix <- function(x = matrix()) {
  
  ##This is modeled after the course example. Sub inv for M and
  ##Inverse for Mean
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##This sets the function and the matrix.  
  
  get <- function() x
  ##This gets the function and the matrix.  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() inv
  ##These get and set th inverse matrix.  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##Smush it into a list.

cachesolve <- function(x, ...) {
  ##solve is an r function that returns the inverse.   
  inv <- x$getinverse()
  ##Computer- go get the inverse and see if it's already been
  ##Computed. If it has, gimme the inverse.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##If it hasn't, go get the original matrix.  
  data <- x$get()
  ##Then invert it  
  inv <- solve(data, ...)
  ##Cache it for later  
  x$setinverse(inv)
  ##and give me the inverse.  
  inv
}