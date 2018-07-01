## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        makeCacheMatrix <- function(mat = matrix()) {
                inverse <- NULL
                setinv <- function(x) {
                        mat <<- x;
                        inverse <<- NULL;
                }
                getinv <- function() return(mat);
                set_inv <- function(invrs) inverse <<- invrs;
                get_inv <- function() return(inverse);
                return(list(set = setinv, get = getinv, setinv = set_inv, getinv = get_inv))
        }

}
## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cach

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheSolve <- function(mat, ...) {
                inverse <- mat$get_inv()
                if(!is.null(inverse)) {
                        message("Get cached matrix")
                        return(inverse)
                }
                cached <- mat$getinv()
                invserse <- solve(cached, ...)
                mat$set_inv(inverse)
                return(inverse)
        }
}


