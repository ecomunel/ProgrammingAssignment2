# <code r>
# =============================================================================
#
# Creates a special "matrix" object that can cache its inverse!
#
makeCacheMatrix <- function(x = matrix()) {
          mat  <- NULL
          set  <- function(y) {
                    x <<- y
                    mat <<- NULL
          }
          get    <- function() x
          setInv <- function(inv) mat <<- inv
          getInv <- function() mat
          list(set = set, get = get,
                setInv = setInv,
                getInv = getInv)
}
#
# =============================================================================
#
# Computes the inverse of the special "matrix" returned by makeCacheMatrix()
# If the inverse has already been calculated, retrieve the inverse from the cache.
#
cacheSolve <- function(x, ...) {
          mat <- x$getInv()
          if(!is.null(mat)) {
                    message("\nUsing inverse matrix from cache!\n\n")
                    return(mat)
          }
          matrix <- x$get()
          mat    <- solve(matrix, ...)
          x$setInv(mat)
          mat
}
#
# =============================================================================
#
# Testing...
#
mat  <- matrix(c(1,6,1,7,7,5,2,4,9),3,3)
cMat <- makeCacheMatrix(mat)
#
ls.str(cMat)
cMat$get()
cMat$getInv()
#
cacheSolve(cMat)
cMat$getInv()
cacheSolve(cMat)
# </code>
