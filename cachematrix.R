## These functions below compute the inverse of a matrix (assuming they exist)
## 

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinve <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinve = getinve)
}


## The function below calculates the inverse of the special matrix made from the function above

cacheSolve <- function(x, ...) {
        m <- x$getinve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}}
            
