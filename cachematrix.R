##  write a pair of functions that cache the inverse of a matrix
## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse

## Cache the matrix instead of calculating one
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

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


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("loading cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
#Example of the run of the function
#> source('~/.active-rstudio-document')
#> x=rbind(c(1,-1/4),c(-1.4,1))
#> m=makeCacheMatrix(x)
#> m$get()
#[,1]  [,2]
#[1,]  1.0 -0.25
#[2,] -1.4  1.00
#> cacheSolve(m)
#[,1]      [,2]
#[1,] 1.538462 0.3846154
#[2,] 2.153846 1.5384615
#> cacheSolve(m)
#loading cached data
#[,1]      [,2]
#[1,] 1.538462 0.3846154
#[2,] 2.153846 1.5384615
