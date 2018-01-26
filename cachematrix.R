## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.

##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse of the matrix
##4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ivs <- NULL
    set <- function(y){
        x <<- y
        ivs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) ivs <<- inverse
    getinverse <- function() ivs
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed)
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ivs <- x$getinverse()
    if(!is.null(ivs)){
        message("getting inverse from the cache")
        return(ivs)
    }
    data <- x$get()
    ivs <- solve(data)
    x$setinverse(ivs)
    ## Return a matrix that is the inverse of 'x'
    ivs
}
