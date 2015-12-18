## Put comments here that give an overall description of what your
## functions do

# The functions below are earmark to produce a inverted matrix from an existing matrix
# and then to cache the result so that it does not have to be calculated each time
# can be reteived on demand more efficiently

## Function Description: Create a special vector object using a passed matrix() object as parameter
### The first fucntion creates a special vector which consist of the list containing a functions to manage the caching
### of the result of the operation performed on the data
### 1. set the value of the vector
### 2. get the value of the vector
### 3. set the value of the inverse of matrix
### 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setminv <- function(minv) m <<- minv
     getminv <- function() m
     list(set = set, get = get,
          setminv = setminv,
          getminv = getminv)
}


## Function description: Returns a inverted matrix that is cache if already inverted using makeCacheMatrix object
### This funcion takes in a Special makeCacheMatrix object and performs an invertion on the pasted matrix or retrieves 
### cached values.
### Note: The assumption of the function is that the passed matrix will always invertable

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getminv()
     if(!is.null(m)) {
          message("Getting cached data")
          return(m)
     }
     data <- x$get()
     ## only perform the solve function operation on the matrix if determant is not zero
     if (!det(data)==0) {
          m <- solve(data, ...)
          x$setminv(m)
     } else {
          message("Matrix cannot be inverted")
     }
     m
     
}
