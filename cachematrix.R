
## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    'set the matrix of the vector'
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    'get the matrix of the vector'
    get <- function() x
    'set the matrix of the inverse'
    setinverse <- function(inverse) m <- inverse
    'get the matrix of the inverse'
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.n

cacheSolve <- function(x, ...) {
    'check if there is cached inverse'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    'if no, get the matrix'
    data <- x$get()
    'get the inverse'
    m <- solve(data,...)
    'set the inverse'
    x$setinverse(m)
    m
}

##test
thematrix <- matrix(rnorm(9), nrow = 3, ncol = 3)

cache <- makeCacheMatrix(thematrix)
cacheSolve(cache)

thematrix
solve(thematrix)
solve (thematrix) %*% thematrix
