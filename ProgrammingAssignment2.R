#MakeCasheMatrix receives a matrix argument into the function then returns it's inverse.
#in Inv <- NULL, initializes variable that will hold the value of the matrix passed to it.
#set the value of a matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

m <- matrix(c(1, 2, 3, 4) ,2,2) #matrix function creates 2 x 2 (square) matrix to the m variable

x <- makeCacheMatrix(m) #passes the matrix variable to the makeCacheMatrix as an argument

cacheSolve(x) # returns the inverse of the matrix.

