## pair of functions to cache inverse matrix of a matrix upon compution
## makeCacheMatrix is the creator function for special kind of matrix
## (like a wrapper) which is able to cache it's inverse matrix.
##
## cacheSolve is computing the inverse matrix in consideration of the cache
## It will use the cached value if available and update the cache upon
## compution of a new value.
##
## please be aware that caching only works when treated with the given
## funtions (in this case cacheSolve)

## creator function for cacheable matrix
makeCacheMatrix <- function(x = matrix()) {
    # initialize cache as empty
    x_inv <- NULL
    
    # setter for matrix
    set <- function(x_new) {
        x <<- x_new
        # invalidate cache
        x_inv <<- NULL
    }
    
    # getter for matrix
    get <- function() x
    
    # setter for cache
    setinverse <- function(inv) x_inv <<- inv
    # getter for cache
    getinverse <- function() x_inv
    
    # return created object
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## computation function for inverse matrix with caching (see above)
cacheSolve <- function(x, ...) {
    #read the cache
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("cache hit")
    } else {
        #invalid, compute new with matrix data
        matrix <- x$get()
        inv <- solve(matrix, ...)
        #store new value in cache
        x$setinverse(inv)
    }
    #return the value
    inv
}
