## This function provides functions to cache the values of the matrix and the inverse matrix
## It also provides functions to retrieve the values of the matrix and inverse matrix
## All of these are called in the cacheSolve function.


makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y){
        m <<- y
        I <<- NULL
    }
    getmat <- function() return(x)
    setinv <- function(inv) I <<- inv
    getinv <- function() I
    
    list(set=set, getmat=getmat, setinv=setinv, getinv=getinv)
}



## This function uses the functions built into makeCacheMatrix.
## It first checks to see if an matrix and inverse matrix exist.
## If an inverse matrix exists and the original matrix has not changed, the function returns the inverse matrix.
## If no inverse exists or if the original matrix has changed, the solves the inverse of the original matrix
## and caches the value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getinv()
    data <- x$getmat()
    if(!is.null(I) && m==data){
        message("Finding your Matrix")
        return(I)
    }
    x$set(data)
    I <- solve(data)
    x$setinv(I)
    return (I)
}
