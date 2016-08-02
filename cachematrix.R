## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that cache the inverse of a matrix.

## The function makeCacheMatrix() creates a special 'matrix' object, which is
## really a list containing a function to 1) set the value of the matrix, 
## 2) get the value of the matrix, 3) set the value of the inverse matrix,
## 4) get the value of the inverse matrix. More simply, makeCacheMatrix()
## creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve() computes the inverse of the special matrix created by 
## makeCacheMatrix(). If the inverse has already been calculated, and the matrix
## has not been changed, then it will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
        
        ## Return a matrix that is the inverse of 'x'
}
