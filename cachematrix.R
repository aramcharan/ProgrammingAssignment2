## makecacheMatrix creates a matrix "object" that can cache its inverse.This can only be done for square matrices.
##This inverse is associated with the matrix using the "<<-" operator to assign a value to the object 
##even though it is in a different environment. 


makeCacheMatrix <- function(x = matrix()) {
  ## create a variable to store the matrix inverse and set it to NULL
  inv<- NULL
  ## set the value of each element of the matrix x
  set<- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## get the value of each element in the matrix x
  get <- function() x
  ## sets the inverse of the matrix to "inv"
  setinv <- function(inverse) inv<<- inverse
  ## get the value of the inverse for the matric x
  getinv <- function() inv
  
  ##store the updated values of "set","get", "setinv" and "getinv" in a list
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of a square matrix. if this has already been done, the function calls on the cached
## data in order to save time with the calculation

cacheSolve <- function(x, ...) {
  ## gets inverse of matrix x
  inv <- x$getinv()
  ## check if inverse of x has already been calculated, if so, return cached data
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  ## get elements of x and store in variable "data"
  data <- x$get()
  ## calculate the inverse of x
  inv <- solve(data)
  ## set this inverse as the inverse of x
  x$setinv(inv)
  ## return the inverse
  inv
}
