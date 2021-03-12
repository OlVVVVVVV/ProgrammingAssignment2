
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( x = matrix() ) {
  
  mymatrix <- NULL

s <- function( y ) {
  x <<- y
  mymatrix <<- NULL
}
get <- function() x
si <- function(sm) mymatrix <<- sm
gi <- function() mymatrix
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  
  mymatrix <- x$getInverse()
  
  if( !is.null(mymatrix) ) {
    return(mymatrix)
  }
  
  my_data <- x$get()
  
  mymatrix <- solve(my_data) %*% data
  
  x$setInverse(mymatrix)
  
  mymatrix
}


