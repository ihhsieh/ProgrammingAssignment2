## The two matrices makeCacheMatrix and cacheSolve combined caches a matrix "a"
## and solves for the inverse of the matrix cached

## The makeCacheMatrix function consists of four distinct functions which 
## set/get the matrix x and its inverse matrix in/from the global environment 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <- NULL
}
get <- function() x
setInv <- function(inverse) inv <<- inverse
getInv <- function() inv
list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function extracts the matrix stored in the global environment and compute
## its inverse using the function "solve"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cache data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    inv
}
