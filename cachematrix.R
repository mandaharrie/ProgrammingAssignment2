## These functions have been created to cache the inverse 
## of a matrix rather than compute it repeatedly.

## My first function (makeCacheMatrix) creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        z <- NULL
        set <- function(y){
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) z <<- inverse
        getinverse <- function() z
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## My second function computes the inverse of the special 
## "matrix" returned by the makeCacheMatrix above.

cacheSolve <- function(x, ...){
        z <-x$getinverse()
        if (!is.null(z)){
                message("getting cached data")
                return(z)
        }
        data <-x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}