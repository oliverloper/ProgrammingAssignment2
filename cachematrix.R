## The functions below are to solve programming assignment 2 in Coursera rprog-31.
## The aim is to create a list that caches an invertable matrix and it's inverse.

## This function creates a list that can cache a matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverted) inv <<- inverted
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
} 

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrimeve the inverse from the cache. 

cacheSolve <- function(x, ...) { 
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
} 