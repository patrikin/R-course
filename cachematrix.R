## makeCacheMatrix and cacheSolve functions: cacheing the inverse of an input matrix x

## Description of what makeCacheMatrix does:
# makeCacheMatrix returns a list that contains four functions: set, get, setinverse and getinverse.
# When makeCacheMatrix is first called, the x has to be initialized with a square invertible matrix, eg someMatrix <- makeCacheMatrix(matrix(c(1:4), nrow = 2))
# Matrix x then lives in the makeCacheMatrix environment.
# The function looks as follows, its body is the same as for the mean example, except that 'mean' was replaced by 'inverse':

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Description of what cacheSolve does:
# When cacheSolve calls the makeCacheMatrix output, eg, cacheSolve(someMatrix), it checks whether there is an inverse matrix stored as m in the  makeCacheMatrix environment, ...
# ... and assigns it to m inside the cacheSolve environment. If m is not NULL, it'll print out its value....
# If m is null, it'll retrieve the matrix from the makeCacheMatrix environment, assigns it to data, calculate its inverse, ....
# ... assign the inverse to m in the makeCacheMatrix environment, and return its value.
# Thus, when cacheSolve is run first, cacheSolve will always calculate the inverse of the matrix,...
#... but afterwards, it will just retrieve the inverse from the makeCacheMatrix environment, until a new matrix is specified with the $set function.
# cacheSolve looks as follows, it's the same as in the mean example, except that it uses the solve instead of the mean function, ....
# .... and 'mean' in the variable names was changed to 'inverse':

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #Change this to 'solve' to create inverse of matrix.
  x$setinverse(m)
  m
}
