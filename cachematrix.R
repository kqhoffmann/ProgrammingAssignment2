## Implementation of a matrix that remembers its solution so that 
## once a particular matrix is solved, its solution does not need
## to be recalculated


## Creates a special matrix that can remember its solution
## 
## Args:
##   x: a matrix object
## 
## Returns:
##   A list containing:
##      setMatrix: a function to set the matrix stored
##      getMatrix: a function to get the matrix stored
##      setInverse: a function to set the inverse of the matrix
##      getInverse: a function to get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        ## Need two variables in this environment
        ##   x: the current matrix, already defined in the function header
        ##   matrix.inverse: the inverse of the matrix. Define this now,
        ##      so that functions defined later have this variable in their scope
        matrix.inverse <- NULL
        setMatrix <- function(new.matrix) {
                ## Set x to the new matrix, and reset the inverse to indicate
                ## that the matrix solution has not been calculated yet
                ## Use the <<- operator, because <- would create a new variable
                ## in this function's environment that would be lost as soon
                ## as the function call ends
                x <<- new.matrix
                matrix.inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(new.inverse) matrix.inverse <<- new.inverse
        getInverse <- function() matrix.inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}


## Returns the solution to a makeCacheMatrix. Checks if the solution has
## already been calculated before resolving the matrix
##
## Args:
##      x: a makeCacheMatrix object
##      ...: additional parameters to pass to the matrix solve() function
##
## Returns:
##      a matrix object that is a solution to the matrix contained within the 
##         the makeCacheMatrix object
cacheSolve <- function(x, ...) {
        ## Check if the matrix has already been computed. If it has, the 
        ## getInverse function will return a non-NULL value, and we will
        ## return this
        current.inverse <- x$getInverse()
        if (!is.null(current.inverse)) {
                message("returning cached inverse")
                return(current.inverse)
        }
        ## Otherwise, we have to compute the solution to the matrix
        current.matrix <- x$getMatrix()
        new.inverse <- solve(current.matrix, ...)
        x$setInverse(new.inverse)
        new.inverse
}
