# makeCacheMatrix function creates a special "matrix" object that can cache 
#its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y)
  {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


# cacheSolve function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if (!is.null(inver))
  {
    message("Showing cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setInverse(inver)
  inver
}

#Output
my_matrix1 <- makeCacheMatrix(matrix(1:4,2,2))
my_matrix1$get()
my_matrix1$getInverse()
cacheSolve(my_matrix1)
my_matrix1$getInverse()
my_matrix1$set(matrix(c(2,2,1,4),2,2))
my_matrix1$get()
my_matrix1$getInverse()
cacheSolve(my_matrix1)

