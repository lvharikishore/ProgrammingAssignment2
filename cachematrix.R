## Couple of methods that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialise the inverse variable
  im <- NULL
  
  ##setter for matrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  ## getter for matrix , returns matrix
  get <- function() x
  
  ## setting inverse of the matrix
  setim <- function(inv) im <<- inv
  
  ## getting the inverse of the matrix
  getim <- function() im
  
  ## list of functions
  list(set = set, get = get,
       setim = setim,
       getim = getim)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getim()
  
  ## return the inverse if its already set in cache
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  
  ## get the matrix
  data <- x$get()
  
  ## computer inverse of matrix
  im <- solve(data, ...)
  
  ## Set the inverse to cache
  x$setim(im)
  
  ## return inverse matrix
  im
}
