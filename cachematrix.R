##  Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL   # im stands for inverse matrix
    
    # matrix
    set <- function(y) { 
        x <<- y
        im <<- NULL
    }
    get <- function() x
    
    # cache
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im

    # create and return a special "matrix" object, which is actually a list 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    # check if the special "matrix" has a cached value
    im <- x$getinverse()  
    if(!is.null(im)) {    # if so, return it
        message("getting cached data")
        return(im)
    }
    
    # if not, get the input matrix, solve the inverse and cache it
    data <- x$get()      
    im <- solve(data, ...)
    x$setinverse(im)
    im
}
