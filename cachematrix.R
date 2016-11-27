# Creates a list containing a function to get/set the value of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Returns the inverse of the matrix based on whether or not the
# result has already been computed.
cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("There's cached data. Retrieving it")
                return (inver)
        }
        data <- x$get()
        inver <- solve(data)
        x$setinverse(inver)
        inver
}