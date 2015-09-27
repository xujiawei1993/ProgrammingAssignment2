## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Define following functions used below:
# * setMtx set the value of a matrix
# * getMtx get the value of a matrix
# * CahInv set the cached value(Inversed Matrix)
# * GetInv get the cached value(Inversed Matrix)
# The variable of "c" stands for "cache" while "y" is the newly input value.
makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        setMtx <- function(y) {
                x <<- y
                c <<- NULL
        }
        getMtx <- function() x

        CahInv <- function(solve) c <<- solve

        GetInv <- function() c

        list(setMtx = setMtx, getMtx = getMtx, CahInv = CahInv, GetInv = GetInv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$GetInv()
        
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        data <- x$GetMtx()
        
        inverse <- solve(data, ...)
        
        x$CahInv(inverse)
        
        inverse
}
