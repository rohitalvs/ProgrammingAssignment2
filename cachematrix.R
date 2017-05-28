## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  invert <- NULL
  
  set <- function(y)
    {
    x <<- y
    invert <<- NULL
  }
  
  get <- function() x
  s_inv <- function(a) invert <<- a
  g_inv <- function() invert
  list(set = set, get = get, s_inv = s_inv, g_inv = g_inv)
}


## Computes inverse of special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) 

{
 
  invert <- x$g_inv()
  
  if(!is.null(invert))
    {
      message("Retrieving the inverse from cache")
      return(invert)
  }
  
  data <- x$get()
  invert <- solve(data)
  x$s_inv(invert)
  invert      
}

# Solution testing case 1. Run to see o/p as part of solution

mat1<- makeCacheMatrix(matrix(2:5, 2, 2))
mat1$get()
mat1$g_inv()
cacheSolve(mat1)
cacheSolve(mat1)

# Solution testing case 2

mat1$set(matrix(c(1, 4, 2, 6), 2, 2))
mat1$get()
mat1$g_inv()
cacheSolve(mat1)
cacheSolve(mat1)
