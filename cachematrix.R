#  Overall Description of makeCacheMatrix and cacheSolve functions:
#
#  Together, the functions makeCacheMatrix and cacheSolve provide a way to 
#  calculate the inverse of a matrix and store the result in the function 
#  environment of makeCacheMatrix.  Since the matrix inverse result is stored 
#  or cached in the function environment, the calling function cacheSolve can 
#  check if there is a cached result first and return this cached result, 
#  instead of recalculating the inverse from scratch.
#
#  Usage example:  
#       makeCacheMatrix should be called first and the result stored in a list: 
#       e.g.
#       > x <- matrix(1:4,2,2)
#       > vfl <- makeCacheMatrix(x)
#
#       cacheSolve can then be called on the vector function list:
#       > cacheSolve(vfl)
#       [,1] [,2]
#       [1,]   -2  1.5
#       [2,]    1 -0.5
#       This first time, cacheSolve calculates the matrix inverse from scratch
#
#       If cacheSolve is called a second time, it checks to see if a matrix 
#       inverse value already exists, and if so, returns the existing value.
#       > cacheSolve(vfl)
#       getting cached matrix inverse
#       [,1] [,2]
#       [1,]   -2  1.5
#       [2,]    1 -0.5
#
#       Note that if the value of x has changed, then makeCacheMatrix must be 
#       called again, otherwise the vfl vector function list environment will 
#       not be updated with the new values of x.



#  Description of makeCacheMatrix function
#
#  Four functions are defined in the makeCacheMatrix function and returned as 
#  a vector list of functions.  These functions query and set the values of 
#  the input data x and its matrix inverse mi
#       set - sets the value of x to y
#       get - query the value of the input x
#       setmatinv - sets the value of the matrix inverse mi
#       getmatinv - query the value of the matrix inverse mi
#
#  The values of x and mi are stored in this function environment.
#  The expected input to makeCacheMatrix is a matrix.

makeCacheMatrix <- function( x = matrix() ) {

     ## check for valid input
     if (!is.matrix(x)){
          message("makeCacheMatrix: Error - input is not a matrix")
          return()
     }
     
     mi <- NULL               ## initialize matrix inverse mi value
                              ## NULL indicates no value is cached
     
     set <- function( y ) {   ## set value of x to y
          x <<- y
          mi <- NULL
     }
     
     get <- function() x      ## get the value of x input data
     
     setmatinv <- function(matinv) mi <<- matinv   ## set/cache value of mi
     
     getmatinv <- function() mi                    ## get cached value of mi
     
     ## return list of functions
     list( set = set, get = get, setmatinv = setmatinv, getmatinv = getmatinv )
}



# Description of cacheSolve function
#
# cacheSolve returns the inverse of the matrix that was input as the data value
# x in the makeCacheMatrix function.  If a matrix inverse value exists (has been
# cached) in the makeCacheMatrix function environment, then that value is 
# returned.  Otherwise, the matrix inverse value is calculated from scratch for
# the input data x and this value is returned.
#
# The expected input to cacheSolve is the output of the makeCacheMatrix function.
#
# Usage examples:
#      > x <- matrix(1:4,2,2)
#      > cacheSolve( makeCacheMatrix( x ) )
#
#      or
#      > x <- matrix(1:4,2,2)
#      > vfl <- makeCacheMatrix( x )
#      > cacheSolve( vfl )

cacheSolve <- function(x, ...) {

     ## check for valid input
     if (!is.list(x)){
          message("cacheSolve: Error - input is not a list")
          return()
     }    
     
     ## check if there is cached value of matrix inverse mi
     ## return matrix inverse if cached
     mi <- x$getmatinv()
     if ( !is.null(mi) ) {
          message( "getting cached matrix inverse" )
          return( mi )
     }
     
     ## there is no cached matrix inverse mi value so get the input data
     ## and calculate
     data <- x$get()
     mi <- solve( data, , ... )
     
     ## cache the value of the matrix inverse mi
     x$setmatinv( mi )
     
     ## return the matrix inverse value
     mi     
}
