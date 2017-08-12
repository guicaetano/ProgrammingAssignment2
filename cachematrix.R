## This function is used to calculate the inverse of a matrix and storing the value
## in the cache in order solve calculation

## makeCacheMatrix create a list with setters and getters for the cachesolve function
## the function also defines m as a with a null value and resets it to null in case a value is writen

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
    setmatrix = setmatrix,
    getmatrix = getmatrix)
}

## cachesolve checks if m is null. if is, it calculates the inverse of the matrix.
## if not it gets the value from m from the list created above.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
        message("getting the inverse of the matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setmatrix(m)
    m
}
