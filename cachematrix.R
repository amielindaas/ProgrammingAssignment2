## Amie Lindaas
## 11/21/14
## Programming assignment 2

## The following two functions will cache the inverse of a matrix
## to use, create a matrix
## matrix <- matrix(rnorm(9), 3, 3)
## then store the matrix as a 'makeCacheMatrix' object
## ghost <- makeCacheMatrix(matrix)
## then solve inverted matrix
## cacheSolve(ghost)
## if you solve a 2nd time, the function gives a cached response

## This makes a cache object (ghost object) to store the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This will check to see if there is something in the cache and 
## if not, it will solve

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    return(i)
}
