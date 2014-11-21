############################################
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse
##   4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## ATTRIBUTES: note that this object has two x and m
  m <- NULL
  
  ## METHODS
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setTheInverse <- function(argInv) m <<- argInv # Metodo honen helburua eztutpanikargi:-!
  getTheInverse <- function() m
  
  list(set = set, get = get,
       setTheInverse = setTheInverse,
       getTheInverse = getTheInverse)
}


##########################################
## The following function calculates the mean of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
##   If so, it gets the inverse from the cache and skips the computation. 
##   Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  m <- x$getTheInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setTheInverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
########################################
## Does this work? Try the following:
## 
# NCols=5
# NRows=5
# A <- matrix(runif(NCols*NRows), ncol=NCols)
# Acache <- makeCacheMatrix(A)
# AInv<-cacheSolve(Acache)
# AInv %*% A
## Isn't the result prety much like Diag(5)? :)