## Coursera R Programming from Johns Hopkins University:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## function takes a matrix as an argument and rreturns a list with 4 functions (see descriptions below):

makeCacheMatrix <- function(x = matrix()) {
  ## here we store the inverse
  inverse <- NULL
  
  ## set = setter function. Assign the x matrix and sets the inverse to NULL
  set <- function(y){
    x <<- y
    inverse <<-NULL
  }
  
  ##get = getter functions. Returns the value of the x matrix
  get <- function(){
    x
  }
  
  ##setInverse = sets the value of inverse
  setInverse <- function (inv){
    inverse <<- inv
  }
  
  ##getInverse = gets the value of inverse
  getInverse <- function (){
    inverse
  }
  
  ##returning the list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##cacheSolve takes as parameter a list of functions constructed with makeCacheMatrix
##it returns the inverse of the original matrix from cache, if previously cached, or by calling solve if it wasn't
cacheSolve <- function(x, ...) {
  ## check if the inverse has been cached already
  if (is.null(x$getInverse())){
    ## printing a message to show that the value returned was calculated
    print("First time - using solve to calculate inverse")
    
    ##solve the inverse and setting the inverse in x
    x$setInverse(solve(x$get()))
  } 
  else{
    ## printing a message to show that the value returned comes form cache
    print("Returning the value from cache")
  }
  
  ## return the value of the inverse 
  x$getInverse()
}
