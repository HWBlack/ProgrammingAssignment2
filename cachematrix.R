## Creates a list that for cache the value of solve 
## based on https://github.com/rdpeng/ProgrammingAssignment2

## makeCacheMatrix -> creates the matrix to be solved
## set -> sets the matrix
## get -> gets the martix
## setsolve -> sets the value of solve
## returnsolve -> returns the value of solve
makeCacheMatrix <- function(x = matrix()) {
      mat <- NULL
      set <- function(y) {
            x <<- y
            mat <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) mat <<- solve
      getsolve <- function() mat
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## cacheSolve -> cacluates the vale of solve for the ChacheMatrix
cacheSolve <- function(x, ...) {
      mat <- x$getsolve()
      if (!is.null(mat)) {
              return (mat)
        }
      data <- x$get()        
      mat <- solve(data)
      x$setsolve(mat)
      mat
}
