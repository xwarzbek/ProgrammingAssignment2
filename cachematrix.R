## Based on the template provided, adapted the example to work with matrices
## The example creates a structured list based on the input matrix, and
##    places it in next level frame using the '<<-' operator.
##    This structured list embeds methods within it, looking a bit pythonesque.
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
  setinv <- function(solve) m <<- solve ## providing results a level up
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
        ## but only for our defined object above, a cache-able matrx
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


#================================================================#
# Thus endeth the assignment part. Now to see what the point is
## testSolve0, below, fails to work, never signaling cached status
###  this doesn't quite create a "curried" function (new vocabulary)
testSolve0 <- function(x = matrix()) {
  intMat <- makeCacheMatrix(x)
  cacheSolve(intMat)
  ## and then it all disappears. How to associate an incoming x?
}
##

## catSolve, where we try to build a session persistent cache
###   
## still half baked.  We want a catalog of "cached-up" matrices to
## be available to look up across
catSolve <- function(x) {
  ## On first invocationin a session, set start conditions
  if (!exists("cachedMatrixCatalog", inherits = TRUE)) {
    cachePosition <- 1
  }
  else {cachePosition <- length(cachedMatrixCatalog) + 1
        }
  
  if (! x %in% cachedMatrixCatalog) {
  ##       ^ list recognition??
  ## 
      intMat <- makeCacheMatrix(x)
  ## Hmm, which arrow is right?
      cachedMatrixCatalog[cachePosition] <<- intMat
  ##                                      ^
  ##          We want our catalog to live | across frames
      outMat <- cacheSolve(intMat)  
      catMat <- makeCacheMatrix(outMat)
      cachedMatrixCatalog[cachePosition + 1] <<- catMat
## we need a defined accuracy to store data. Example
##> bob
##       [,1] [,2]      [,3]
## [1,] -0.2   -3  2.666667
## [2,]  0.4    3 -2.666667
## [3,] -0.2   -1  1.000000
##
##> bob[2,1] - 0.4
## [1] -8.326673e-16 
## 
## Will in general casue lots of headaches. 
## We want our serach to be somewhat fuzzy
  
      return(outMat)
      } 
  else {
        ## output catalog item's inverse
    cachedMatrixCatalog[catIndex]$getInv
      }
}
##
