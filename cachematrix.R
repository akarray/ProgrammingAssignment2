## The goal of this pair of functions is to cache the inverse of a matrix.



## makeVector creates a special "matrix", which is really a matrix containing a function to
## set the value of the squared matrix
## get the value of the squared matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the matrix created with the makeCacheMatrix function. 
## However, it first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse of the matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(m) {
    ## Return a matrix that is the inverse of 'm'
    inv <- m$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data)
    m$setinverse(inv)
    inv
    
}


## This code is used just for test purpose
## m <- makeCacheMatrix(matrix(1:4,2,2))
## t <- cacheSolve(m)
