# R programming, programming assignment2, lajordan MMXV

# manipulates scoping rules of R to preserve the local
# state of an R object by creating a matrix object that
# can cache its inverse;

# scoping rules in R use a hierarchical/cascaded search 
# order to determine how a value is associated with a
# free variable in a function;

# the superassignment operator allows alteration of
# free variables through successive layers upward from 
# the defining environment, through potentially multiple
# parent environments, to the top level environment; 
# an empty environment produces an error;

# 'makeCacheMatrix' creates an object that instantiates
# an empty numeric vector that holds local variables & 
# functions designed to operate on the object

makeCacheMatrix <- function(x = numeric()) {
  # initialize the vector to hold the contents of cache
  myCache <- NULL
  
  # from the free variable, y, assign the inverse of the 
  # input matrix to setMatrix
  setMatrix <- function(y) {
      # assign to the vector x of the parent environment
      # the input matrix y
      x <<- y
      myCache <<- NULL
  } # end setMatrix
  
  # retrieve/return the contents of the matrix x
  getMatrix <- function() 
      x
  
  # 'setInverse' stores in cache the vector passed to it;
  # sets cache equal to the inverse of the matrix x;
  # the 'solve' function returns the inverse of its
  # argument
  setInverse <- function(solve) myCache <<- solve
  
  # 'getInverse' returns the contents of cache
  getInverse <- function() 
      myCache
  
  # collection of methods that operate on the object
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
      setInverse = setInverse, 
      getInverse = getInverse)
} # end makeCacheMatrix


# R programming, programming assignment2, lajordan MMXV

# 'cacheSolve' compares the matrix argument to the contents 
# of cache; 
# if the inverse has already been calculated AND the matrix
# has not changed, retrieves the contents from cache

cacheSolve <- function(x, ...) {
      myCache <- x$getInverse()
  
  # if the returned cache contains unchanged data, message
  # that those contents are being retrieved/returned
  if(!is.null(myCache)) {
      message("getting cached data")
      return(myCache)
  } # end if
  
  # if the cache is empty, the local function assigns the 
  # instantiated vector to the local variable, 'data', whose 
  # inverse is then returned to the calling function & then 
  # to cache, myCache, as updated cache data
  data <- x$getMatrix()
  myCache <- solve(data, ...)
  x$setInverse(myCache)
  myCache
} # end cacheSolve