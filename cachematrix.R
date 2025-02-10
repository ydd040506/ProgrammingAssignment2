## My functions are supposed to cache the inverse of a matrix.
## This function creates the "matrix" object and stores it in the cache

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  update <- function(y) {
    #assign the new matrix to x
    x <<- y
    inverse <<- NULL
  }
  retrieve <- function() {
    #return the matrix
    return(x)
  }
  storeInverse <- function(inv) {
    #store the inverse of the matrix
    inverse <<- inv
  }
  retrieveInverse <- function() {
    #return the inverse of the matrix
    return(inverse)
  }
  #return a list of the functions
  return(list(update = update, retrieve = retrieve, storeInverse = storeInverse, retrieveInverse = retrieveInverse))

}

## this function calculates the inverse of the matrix entered above 
## if it is not cached yet, and returns the inverse directly if it is cached

cacheSolve <- function(a, ...) {
        #return the inverse if it is already cached
        inverse <- a$retrieveInverse()
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        #if the inverse is not cached
        #we first retrieve the matrix
        currentMatrix <- a$retrieve()
        #then calculate the inverse
        inverse <- solve(currentMatrix, ...)
        #then store the inverse in the cache
        a$storeInverse(inverse)
        #return the inverse
        return(inverse)
}
