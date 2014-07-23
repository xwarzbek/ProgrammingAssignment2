## Based on the template provided, adapted the example to work with matrices
## The example creates a structured list based on the input matrix, and
##    places it in next level frame (?) using the '<<-' operator.
## In this version, I don't like the action, since the output is brought to
##    "cacheSolve" separately. A catalog of matrices tested in the environment,
##    and the calculated inverses would be even better to check
## More work with '<<-' is needed.

## makeCacheXXX : creating a list object that includes the applicable operators
##                These are accessed as structure members with the $ operation
## Assumptions  : inverting square matrices, all test cases are invertible
## Improvements : test the assumptions
##                create functions of functions that determine methods by input 
##                  structure, and future function call
## 

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


## cacheFunction : call the made objects (see above)
## Assumptions   : objects called on exists and has been treated by "makecacheMatrix"
## Improvements  : see that the structure is correct for the input.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## testSolve, below, fails to work
###  this doesn't create a "curried" function (new vocabulary)
testSolve <- function(x = matrix()) {
  intMat <- makeCacheMatrix(x)
  cacheSolve(intMat)
}
##
