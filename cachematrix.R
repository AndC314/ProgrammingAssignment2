##Must assume that all matrix are square and invertible, but anyway
##will write some code to check
##makeCacheMatrix transform input data into a suitable matrix
##the matrix below is a known invertible matrix
##the product of the matrix with its inverse must yield the identity matrix

matA = matrix(c(-1,1,3/2,-1),2,2)
matB <- solve(matA)
matA %*% matB
matB %*% matA


makeCacheMatrix <- function(x = matrix()) {
  ## this row makes sure that the dimensions of the matrix are square
  ## throws an error otherwise
  dim(x) <- c(sqrt(length(x)), sqrt(length(x)))
  #Last check for the matrix, it must not be singular (determinant == 0)
  #this condition is sufficient for it not being invertible
  
  if (det(x)==0){
    print('The Matrix is singular')
    break
  }
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function is perfectly analogue to the one given
## in the example for vector manipulation except some names are changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
