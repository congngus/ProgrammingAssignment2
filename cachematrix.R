## Coursera
## R Programming by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo.
## Johns Hopkins University.
## Programming Assignment 2
##_____________________________________________________________________##

## A pair of functions that creates a "special matrix" object that can cache its inverse
## and computes the inverse of the "special matrix".

## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( inputMatrix = matrix() ) {

    ## Initialize inverse maxtrix.
    inverseMatrix <- NULL

    ## Method to set new input matrix.
    setInputMatrix <- function( matrix ) {
        ## Set new input matrix.
        inputMatrix <<- matrix

        ## Clear did inverse matrix.
        inverseMatrix <<- NULL
    }

    ## Method the get input matrix.
    getInputMatrix <- function() {
        ## Return input matrix.
        inputMatrix
    }

    ## Method to set inverse of input matrix.
    setInverseMatrix <- function(inverse) {
        ## Set inverse matrix.
        inverseMatrix <<- inverse
    }

    ## Method to get inverse of matrix.
    getInverseMatrix <- function() {
        ## Return inverse matrix.
        inverseMatrix
    }

    ## Return this special object as a list.
    list(setInputMatrix   = setInputMatrix,
         getInputMatrix   = getInputMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}

## 2.cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(specialMatrix, ...) {

    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- specialMatrix$getInverseMatrix()

    ## Just return the inverse matrix if It's already exist.
    if( !is.null(inverseMatrix) ) {
        message("Getting cached data.")
        return(inverseMatrix)
    }

    ## Get the matrix from special object.
    input <- specialMatrix$getInputMatrix()

    ## Calculate inverse matrix.
    inverseMatrix <- solve(input)

    ## Set inverse to special object.
    specialMatrix$setInverseMatrix(inverseMatrix)

    ## Return inverse matrix.
    inverseMatrix
}
