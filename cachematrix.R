## Usage: > m <- makeCacheMatrix(matrix(rnorm(25), 5))
##        > cacheSolve(m)
##        > cacheSolve(m) # this should get cached data
## Notes: I changed the variables (x, y, ...) into
##        more readable names because the old ones didn't
##        help any in reading the code.

## Function: makeCacheMatrix(matrix())
## Returns: A list of handy funs for setting and 
##          retrieving values of a matrix.
makeCacheMatrix <- function(my_matrix = matrix()) {
    cached_inverse <- NULL
    set <- function(new_matrix) {
        my_matrix <<- new_matrix
        cached_inverse <<- NULL
    }
    get <- function() my_matrix
    setinverse <- function(new_inverse) {
        cached_inverse <<- new_inverse
    }
    getinverse <- function() cached_inverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function: cacheSolve(makeCacheMatrix(), ...)
## Arguments: - makeCacheMatrix() - object returned
##              by calling makeCacheMatrix(matrix())
##            - ... - additional arguments for the 
##              solve() R function
## Returns: matrix() - Inverse of the matrix stored
##          in the makeCacheMatrix object
cacheSolve <- function(cacheObj, ...) {
    # Return a matrix that is the inverse of the matrix
    #   stored in the makeCacheMatrix object
    cached_inverse <- cacheObj$getinverse()
    if (!is.null(cached_inverse)) {
        message("getting cached data")
        return(cached_inverse)
    }
    my_matrix <- cacheObj$get()
    inverse <- solve(my_matrix, ...)
    cacheObj$setinverse(inverse)
    return(inverse)
}
