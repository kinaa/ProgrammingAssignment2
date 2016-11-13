# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly.

#This function creates a special "matrix" object that can cache its inverse.
#It creates a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<-solve
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        # checking if the inverse has already been calculated
        if(!is.null(m)) {
            # get it from the cache and returns it, no computation needed
            message("getting cached data")
            return(m)
        }
        # it's ot cached, then invers needs to be calculated
        data  <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}