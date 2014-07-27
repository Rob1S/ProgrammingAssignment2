
## This function creates a chache "matrix" object of the inverse of matrix "x".
## It is assumed that matrix "x" is always invertible
## The inverse is "stored" in object inv
## The matrix x is "stored" in object src, so we can check if it is changed or not.

makeCacheMatrix <- function(x = matrix()) {
        inv <<- solve(x)
        src <<- x
}


## This function computes the inverse of given "matrix".
## If the matrix has not changed and the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
## First check if x is not changed
        check <- is.matrix(x) && is.matrix(src) && dim(x) == dim(src) && all(x == src)
## Then check if the invertion has been done
        if (check)  
                if(!is.null(inv)) {
## Return a matrix that is the inverse of 'x'
                message("getting cached data")
                return(inv)
        }
## Else (if x has changed or the inversion has not taken place, inverse x 
        inv <<- solve(x)
        return(inv)
}
