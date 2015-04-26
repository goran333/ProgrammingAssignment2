## Program to cache inverse of a matrix

## Create a wrapper around a matrix to cache the inverse value for
## faster access later.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## initialize inverse to NULL
    
    ## change the underlying matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL ## since matrix has changed, reset inverse to NULL
    }
    ## get the underlying matrix
    get <- function() {
        x
    }
    ## set the inverse value
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    ## get the cached inverse value
    getinverse <- function() {
        inv
    }
    
    ## return the wrapped matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Get inverse of the matrix 'x$get()'. Uses the cached value if it is 
## available for faster execution.
cacheSolve <- function(x, ...) {
    ## Get the cached value of inverse matrix
    inv <- x$getinverse()
    
    ## If cached value not NULL, then just return it
    if(!is.null(inv)) {
        message("Getting cached inverse matrix")
        return(inv)
    }
    
    ## No cached value, available, calculate the inverse of the underlying matrix
    data <- x$get() ## get the underlying matrix
    inv <- solve(data) ## calculate the inverse (ASSUMPTION: matrix is invertible)
    
    ## Store the inverse in the cached value for future calls
    x$setinverse(inv)
    
    ## Return the inverse matrix
    inv
}

## Test code to a simple functional test
testCacheSolve <- function() {
    ## Create a 2x2 matrix 
    x = rbind(c(1, -1/4), c(-1/4, 1))
    
    ## Get the inverse
    xInv = cacheSolve(makeCacheMatrix(x))
    
    ## Confirm that an identity matrix is produced by multiplying x & xInv
    if (all(diag(2) == (x %*% xInv))) {
        message("It worked!")
    } else {
        message("ERROR")
    }
}
