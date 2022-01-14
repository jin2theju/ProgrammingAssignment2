## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a special matrix that can cache its inverse for the input 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set=set, get=get,
             setInverse = setInverse,
             getInverse = getInverse)
       }

}


## cacheSolve is a function that computes the inverse of the matrix returned by makeCacheMatrix (above).

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached matrix")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
