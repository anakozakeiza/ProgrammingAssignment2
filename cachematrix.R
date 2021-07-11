## These functions, "makeCacheMatrix" and "cacheSolve", are able to cache the inverse of a matrix

## "makeCacheMatrix" is a function that creates a special "matrix" object that can cache its inverse given a 
##  square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## set the value of the matrix
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }

    ## get the value of the matrix
    get <- function() x ## return the matrix
    
    ## set the value of the inverse
    set_inverse <- function(inverse) inv <<- inverse
    
    ## get the value of the inverse
    get_inverse <- function() inv
    
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


##  "cacheSolve" computes the inverse of the special "matrix" created by the "makeCacheMatrix" function above. 
##  However, if the inverse for a matrix has already been calculated, and the matrix remains the same, this function 
##  should retrieve it from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    
    ## check if the inverse has already been calculated. If so, retrieves it from the cache
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    ## otherwise, calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}

