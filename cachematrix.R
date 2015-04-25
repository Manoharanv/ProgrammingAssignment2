## Time consuming repeated computations can be avoided by caching the Matrix inversion
## by use of the following two functions.
## makeCacheMatrix creates function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m_inv <<- inverse
    getinverse <- function() m_inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function computes and returns the inverse of the matrix.
# Computation is skipped if the inverse has been already computed and cached.
# This is on the assumption that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    m_inv <- x$getinverse()
    if(!is.null(m_inv)) {
        message("getting cached data.")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data)
    x$setinverse(m_inv)
    m_inv
}

## Sample run:
##>  x <- rbind(c(1, -1/8), c(-1/8, 1))
##>  m <- makeCacheMatrix(x)
##>  m$get()
##       [,1]   [,2]
##[1,]  1.000 -0.125
##[2,] -0.125  1.000
##> 
## No cache in the first run
##> cacheSolve(m)
##          [,1]      [,2]
##[1,] 1.0158730 0.1269841
##[2,] 0.1269841 1.0158730
##> 
## Retrieves from the cache
##> cacheSolve(m)
##getting cached data.
##          [,1]      [,2]
##[1,] 1.0158730 0.1269841
##[2,] 0.1269841 1.0158730
##> 
