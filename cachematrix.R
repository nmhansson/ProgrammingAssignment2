## cacheMatrix.R contains functions to calculate and return the 
## inverse of a non-singular (i.e. invertible) matrix. The value 
## is cached so that if it has already been calculated, then
## the cached value is returned instead of again computing
## the inverse
##

#################################################
## Example use:
## > idmat <- matrix(c(1,0,0,1),2,2)
## > vec <- makeCacheMatrix(idmat)     
## > invMat <- cacheSolve(vec)    ## Here the inverse is calculated
## > invMat <- cacheSolve(vec)
## getting cached inverse - Thus here the cached inverse is retrieved
## instead of calculating the inverse explicitly.
## > invMat
##      [,1] [,2]
## [1,]   1    0
## [2,]   0    1
## It is possible to modify the matrix in vec
## > idmat2 = 0.5*idmat
## > idmat2
##      [,1] [,2]
## [1,]  0.5   0
## [2,]   0   0.5
## > vec$set(idmat2)
## > cacheSolve(vec) ## Here the inverse has to be calculated
##                      since the matrix is newly set in vec
##     [,1] [,2]
## [1,]  2    0
## [2,]  0    2
################################################

## function makeCacheMatrix takes a (non-singular) matrix argument and 
## and returns a special matrix object, which is a list of functions. 
## These functions return and set the matrix, as well as return cached inverse of the matrix 
## or set it to NULL. 
## 1) set - sets the matrix in the list. This function will reinitialize the 
##    special matrix object, setting the matrix in the list to the one given 
##    by the user and the cached inverse to NULL. 
## 2) get - returns the matrix in the list.  
## 3) setInverse - calculates the inverse of the matrix in the list and set the 
##    calculated inverse to the variable inv.
## 4) getInverse - return inv (the cached value of the inverse) or if inverse has not been
##    calculated then NULL is return.

makeCacheMatrix <- function(mat = matrix()) {
        ## set inverse to NULL
        inv <- NULL
        ## function set sets the matrix mat to y and the sets the value of inv to NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        ## function get returns the matrix mat
        get <- function() mat
        ## function setInverse sets inv to the matrix invMat 
        setInverse <- function(invMat) inv <<- invMat
        ## function getInverse returns the inverse Matrix inv
        getInverse <- function() inv

        ## The special matrix object consisting of a list of the four functions is returned.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## function cacheSolve takes the special matrix object x as produced by makeCacheMatrix as 
## argument, and either returns the cached inverse of the matrix or calculates the inverse 
## if it has not been previously calculated.

cacheSolve <- function(x) {
        
        ## get the inverse from the special matrix object x 
        inv <- x$getInverse()
        
        ## If the inverse already exists in x then return the cached value inv
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        ## If the inverse has not been cached, then
        ## get the matrix from the special matrix object x
        mat <- x$get()
        ## calculate inverse
        inv <- solve(mat)
        ## set inverse to calculated value in the list of functions x
        x$setInverse(inv)
        ## return the inverse Matrix inv
        inv
}


