## Creates a list that for cache the value of solve 
## based on https://github.com/rdpeng/ProgrammingAssignment2

## makeCacheMatrix -> creates the matrix to be solved
## set -> sets the matrix
## get -> gets the martix
## setsolve -> sets the value of solve
## returnsolve -> returns the value of solve

makeCacheMatrix <- function(cMatrix = matrix()) {
      # setting the solveResult to NULL to show solve has not yet been calculated
      solveResult <- NULL
      set <- function(y) {
            # setting cMatrix to y as global
            cMatrix <<- y
            # setting the solveResult to NULL 
            #to show solve has not yet been calculated as global
            solveResult <<- NULL
      }
      get <- function() cMatrix
      # calculating the invrese and setting to to solveResult as gloval
      setsolve <- function(solve) solveResult <<- solve
      getsolve <- function() solveResult
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## cacheSolve -> cacluates the vale of solve for the ChacheMatrix
cacheSolve <- function(cMatrix, ...) {
      # getting solveResule
      solveResult <- cMatrix$getsolve()
      # is solveResult NULL
      if (!is.null(solveResult)) {
            message("cached data")
            # no: return the cached value
            return (solveResult)
      }
      # if solve result is not NULL will not reach this point due to
      # the return statement above
      # solve result is NULL
      data <- cMatrix$get()        
      solveResult <- solve(data)
      # calling setsolve to set the result as a global
      cMatrix$setsolve(solveResult)
      solveResult
}