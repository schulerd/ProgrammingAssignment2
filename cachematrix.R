#This functions cache the inverse of a matrix

# Creates a cacheable Matrixobject
makeCacheMatrix <- function(x = matrix()) {
  # Initialize 
  m<-NULL
  
  # Method to set
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  # Method the get
  get <- function() x

  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() m
  
  # Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#Returns the inverse of a matrix. If this already has been taken, the cached object will be returned

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}



