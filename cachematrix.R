
##Two functions, makeCacheMatrix and cacheSolve() to implement a cached inverse
##matrix.

## Function makeCacheMatrix:
##     Input:  square matrix
##     Returns: list of set/get functions for square matrix and its inverse
##     Summary: sets/gets a square matrix and its inverse. The output
##              from makeCacheMatrix is used as input to the cacheSolve function.

##     Pre-condition:  The input matrix is an n x n matrix (square matrix), which
##                     has an inverse.  WARNING: No error checking is performed
##                     on the input matrix to verify that it is a square matrix.
makeCacheMatrix <- function(x = matrix()) {
      #initialize the inverse matrix to NULL;
      #inv_x is global to the makeCacheMatrix
      inv_x <<- NULL
      #set the matrix
      set <- function(y){
            #Initialize the global variables x (the matrix) and inv_x
            #(the inverse of matrix x) to NULL.
            x<<-y
            inv_x<<-NULL

      }
      #get the matrix
      get <- function()x

      #set the inverse matrix
      setinverse <- function(i_x) inv_x <- i_x

      #get the inverse matrix
      getinverse <- function() inv_x
      list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Function cacheSolve:
##     Input:  The output from makeCacheMatrix(); a list of all the set/get
##             functions.
##     Returns: The inverse of the input matrix (input to makeCacheMatrix)
##     Summary: If the inverse of the square matrix already exists, use
##              makeCacheMatrix's getinverse() to retrieve the
##              inverse matrix.  If the inverse of the matrix does not
##              exist/not defined, then cacheSolve() calls the solve()
##              function to compute the inverse matrix.
cacheSolve <- function(x, ...) {
         #retrieve the value of the inverse matrix of x if it has already
         #been calculated or not null.
         inv_x <- x$getinverse()
         if( ! is.null(inv_x)){
               return(inv_x)
         }

         #Otherwise, calculate the inverse of matrix x using solve()
         #First, retrieve the matrix x
         data <- x$get()
         inv_x <- solve(data)
         #update the value for the inverse matrix, now that we've calculated it.
         x$setinverse(inv_x)
}
