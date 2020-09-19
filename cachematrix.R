
#Matrix Cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL     #Initialization
  set <- function(y) {    #Set the Matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x    #Getting the matrix 
  setInverse <- function(Inverse) m <<- Inverse   #Set the inverse of the matrix
  getInverse <- function() m    #Get the inverse of the matrix
  #The list is storing methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


cacheSolve <- function(x, ...) {
  #Return the inverse of the matrix
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()  #Get the matrix
  m <- Inverse(data, ...)
  x$setInverse(m)  #Set the inverse
  m
}
