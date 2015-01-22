## cacheMatrix.R contains function to calculate and return the 
## inverse of a non-singular matrix. The value is cached
## so that if it has already been calculated, then
## the cached value is returned instead of again computing
## the inverse
##

####################################
## Example use:
## > idmat <- matrix(c(1,0,0,1),2,2)
## vec <- makeCacheMatrix(idmat)
## > invMat <- cacheSolve(vec)
## > invMat <- cacheSolve(vec)
## getting cached inverse
## invMat
##      [,1] [,2]
## [1,]   1    0
## [2,]   0    1
## 
## In the first use of cacheSolve the inverse is 
## calculated and cached. In the second use of 
## cacheSolve the inverse is not calculated, 
## instead the cached value is returned.
####################################

## function makeCacheMatrix takes a (non-singular) matrix argument and 
## and returns a list of functions: 
## 1) set - sets the list 
## 2) get - return the list
## 3) setInverse - calculates the inverse of the input matrix and set assign the calculated inverse
##    to the variable inv.
## 4) getInverse - return inv (the cached value of the inverse) or if inverse has not been
##    calculated then NULL is return.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## function cacheSolve takes list argument as produced by makeCacheMatrix, and
## either returns the cached inverse or calculates the inverse if it has not been 
## previously calculated.

cacheSolve <- function(x) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}


