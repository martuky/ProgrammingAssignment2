# This function is generated similarly to Example mean vector
# provided on the course. We generate a matrix to stay
# Cache to apply the inverse matrix calculation on it

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  # Function which will store the matrix of which is to calculate the inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Function that "calls" the matrix on which to calculate the inverse
  get <- function() return(x)
  # Function set to be calculate the parameter "solve " that computes the inverse
  setinv <- function(solve) m<<- solve
  # Function returns the inverse
  getinv <- function() return(m)
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# Calculating the inverse matrix we have prepared above. The parameter passed to this function is the running makeCacheMatrix ,
# i.e, the matrix output function that creates the array in cache

cacheSolve<- function(x, ...) {
# Check if the inverse is calculated in cache
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Should not be calculated in memory, we called the matrix on which is to calculate the inverse 
  inv <- x$get()
  # Calculamos la inversa de esta y la almacenamos en cache
  m <- solve(inv, ...)
  x$setinv(m)
  return(m)
}
