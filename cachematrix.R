## The following set of functions are used to invert a square matrix, with additional functionality to 
## cache and reuse previously inverted matrix values.

## The function 'makeCacheMatrix' takes a square matrix as input and returns a list of functions
## which could be used to fetch the original matrix (using get), save the original matrix (using set),
## lookup the inverted matrix (using getinv) and save the inverted matrix (using setinv)

makeCacheMatrix <- function(x = matrix()) {
  
  # Check if this is a square matrix
  if (nrow(x) != ncol(x)) {
    message ("Not a square matrix")
    return (NULL)
  }
  
  # Clear out the initial inverted matrix 
  matrix_inv <- NULL
  
  # This function saves the original matrix
  set <- function (y) {
    x <<- y
    matrix_inv <<- NULL
  }
  
  # This function returns the saved original matrix
  get <- function () x
  
  # This function saves the inverted matrix 
  setinv <- function (inv) matrix_inv <<- inv
  
  # This function returns the saved inverted matrix
  getinv <- function () matrix_inv
  
  # Finally, the list of functions which save and fetch the original and inverted matrices are returned.
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## The 'cacheSolve' function takes as input the list of functions produced by the previous function 
## 'makeCacheMatrix' and returns the inverse of the matrix specified therein. The function checks the 
## 'matrix_inv' variable to determine if the inverse was previously computed and could be gotten from 
## the cached value instead of computing afresh. If the inverse of this matrix is being computed for the
## first time, we check the determinant to ensure that this is not a singular matrix, which cannot be
## inverted. We then save the inverted matrix in the cache for future use and return it.

cacheSolve <- function(x, ...) {
  
  # Get the matrix inverse from the defining environment
  matrix_inv <- x$getinv()
  
  # If the matrix inverse exists (not NULL), it probably was computed earlier and cached.
  # So, do not bother computing the inverse again, but just return the cached value.
  if (!is.null(matrix_inv)) {
    message ("Getting cached matrix inverse")
    return (matrix_inv)
  }
  
  # Get the original matrix and check its determinant to see if it singular
  matrix <- x$get()
  if (det(matrix) == 0) {
    message ("Unable to get the inverse of a singular matrix")
    return (NULL)
  }
  
  # Get the inverse of this matrix using the 'solve' function. Relax the default tolerance settings as shown
  matrix_inv <- solve(m, tol=1e-20, ...)
  
  # Save the inverse matrix in the cache, which is in defining environment
  x$setinv (matrix_inv)
  
  # Return the newly computed inverse matrix
  return (matrix_inv)
}
