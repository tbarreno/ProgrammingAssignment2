#
# DataScience R-Programming Assignment 2
#
# Optimization functions.

#
# makeCacheMatrix
#
# This function creates a vector with setters and getters for a Matrix
# and its inverse.
#
# Optional param 'the_matrix' : initialize the Matrix.
#
makeCacheMatrix <- function(the_matrix = matrix()) {

    # Clears the matrix
    inverse <- NULL

    # set( y=matrix ) : sets the matrix from another one
    set <- function( initialmatrix ) {
            the_matrix <<- initialmatrix
            inverse <<- NULL
    }

    # get() : returns the matrix
    get <- function() the_matrix

    # setinverse( matrix ) : sets the inverse
    setinverse <- function( inversematrix ) inverse <<- inversematrix

    # getinverse() : returns the matri
    getinverse <- function() inverse

    # returns a list with four the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


#
# cacheSolve
#
# Calculates the inverse of a "CacheMatrix": if the CacheMatrix has
# a previously calculated inverse, it just returns it. If the CacheMatrix
# doesn't has inverse, it calculates it and set the value inside the
# CacheMatrix.
#
# Param 'the_matrix' : The CacheMatrix vector.
#
cacheSolve <- function(the_matrix, ...) {

    inverse <- the_matrix$getinverse()

    if( !is.null(inverse) ) {
            message("Getting cached data")
            return(inverse)
    }

    data <- the_matrix$get()

		# Calculate the inverse
    inverse <- solve(data, ...)

    the_matrix$setinverse(inverse)

		## Return a matrix that is the inverse of 'the_matrix'
    inverse
}
