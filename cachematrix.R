## This function should use the <<- operator to create a matrix which can cache/it stores
## its inverse.
## It adapts the example offered in the assignment, as recommended in suggestions, 
## and in DanieleP detailed guide, on which these explanations are based. 

makeCacheMatrix <- function(x = matrix()) {
## defines the main function  
  m <- NULL
  set <- function(y = matrix) {
    x <<- y
    m <<- NULL
  }
## defines a function which changes the x matrix with the y matrix in the main function
  get <- function()
    x
##returns the matrix from the main function
  setsolve <- function(solve)
    m <<- solve
## stores the value of m in the fain function
  getsolve <- function()
    m
## returns the value 
  list(
    set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
##stores the secondary functions into the main one
}

## calculates the invers of the matrix previously defined

cacheSolve <- function(x, ...) {
##defines the main function
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
##checks if m is NULL
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
## it calls the stored matrix, compute its inverse and stores it
}
## Return a matrix that is the inverse of 'x'
## tested it by defining an object with the value of
## MakeCacheMatrix used on a 2*2 matrix,then getting the matrix
## and using cacheSolve upon it. 
