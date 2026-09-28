#Coursera R Programming - Programming Assignment 2: Lexical Scoping

#Caching the Inverse of a Matrix

# makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}



# cashSolve() will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
        
}

#Example
m <- matrix(c(1,  0, 2, 2, -1, 3, 4,  1, 8), nrow = 3, ncol = 3)
cacheM<-makeCacheMatrix(m)
cacheSolve(cacheM)
solve(m)
