## Put comments here that give an overall description of what your
## functions do

## ============ MY COMMENTS ==============
## makeCacheMatrix works in a similar way as the example makeVector function
## it returns a list of functions that set the matrix, get the matrix, set the inverse and get the inverse, in this order
## cacheSolve also works in a similar way as the example but instead of calculating the mean it evaluates the inverse through solve function

## Write a short comment describing this function

## ============ MY COMMENTS ==============
## m <- NULL starts m for future use
## set is a function which defines the matrix x as y in this environment and m as NULL
## get is a function which gets the matrix x
## setinverse is a function where sets the inverse to m
## getinverse gets m
## last line returns the list

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## Write a short comment describing this function

## ============ MY COMMENTS ==============
## cacheSolve is a function that takes as argument a matrix x
## the inverse matrix is stored in m and if it is NULL matrix receives the matrix that should be evaluated
## and m receives the inverse which is cached by x$setinverse(m) and finally cacheSolve returns m
## otherwise, if m is not NULL it means that the inverse matrix is cached and the cached value is returned.


cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}
