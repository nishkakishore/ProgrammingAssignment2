## The following functions help calculate the inverse of 
## a matrix and cache the inverse of the matrix. Since 
## matrix inversion can be a time consuming process, caching
## the inverseof a matrix helps speed up computations.

## The makeCacheMatrix function below creates a matrix object,
## stores this matrix in the parent environment and can also cache 
##  the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function(y) {
                x <<- y
                mat_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mat_inv <<- solve
        getinverse <- function() mat_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## The cacheSolve function below returns the inverse of the matrix
## created in the makeCacheMatrix function. This function 
## first tries to retrieve the inverse of the matrix object that 
## was passed to it (by retrieving the inverse matrix cached in 
## the makeCacheMatrix objects environment). If NULL is returned 
## when attempting to retrieve the cached inverse matrix, then the
## function retrieves the matrix, calculates the inverse of the 
## matrix and returns this inverse value to the user as well as 
## saves it to the parent environment. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getinverse()
        if(!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }
        data <- x$get()
        mat_inv <- solve(data, ...)
        x$setinverse(mat_inv)
        mat_inv
}

