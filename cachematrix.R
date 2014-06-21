## This is the second programming assignment in Coursea's R Programming class.  This code calculates and cache's the 
## inverse of a matrix.   Its an exercise to demonstrate a technique for saving time-consuming computations by
## saving state inside an R Object.


## makeCacheMatrix creates a special "matrix" object which is really a list containing functions to 
## set / get a matrix and get / set the inverse of a matrix (using getinverse and setinverse functions)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and 
##  sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve (data, ...) %*% data
        x$setinverse(m)
        m
}