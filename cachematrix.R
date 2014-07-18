## The two functions makeCacheMatrix and cacheSolve compute the inverse
## of an invertible matrix and cache the results so that if needed the
## the inverted matrix can be recalled without recomputing the inverse.

## This function accepts a matrix as an argument. It then creates a list of methods:
## set allows the user to assign the matrix after calling makeCacheMatrix ();
## get returns the stored matrix; setinverse sets the variable m to the inverted matrix;
## getinverse returns the inverted matrix

makeCacheMatrix <- function(stored_matrix = matrix()) {
  m <- NULL
  set <- function(y) {
    stored_matrix <<- y
    m <<- NULL
  }
  get <- function() stored_matrix
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function accepts an instantiation of makeCacheMatrix and solves for the inverted
## matrix. It first checks to see if the inverse has already been calculated by attempting
## to retrieve the inverse using the getinverse() method. If the inverse exists it
## displays the message "getting chached data" and exits displaying the inverted matrix.
## If the inverse does not exist it assigns to the variable 'data' the stored matrix from
## makeCacheMatrix using the get() method, it then solves for the inverse and assigns the answer
## to the private variable m, it then stores the inverted matrix using the setinverse() method
## and exits, displaying the inverted matrix.
## 

cacheSolve <- function(supermatrix, ...) {
  m <- supermatrix$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- supermatrix$get()
  m <- solve(data, ...)
  supermatrix$setinverse(m)
  m
}
