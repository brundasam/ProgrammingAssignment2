## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix defines functions to set/get matrix and also 
## cache the Inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(InvMatrix) m <<- InvMatrix
    getInvMatrix <- function() m
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## cacheSolve first gets the cached inverse matrix but if it is not
## cached then it computes the inverse and caches it before returning
## this function assumes the matrix is inversible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInvMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInvMatrix(m)
    m
}
