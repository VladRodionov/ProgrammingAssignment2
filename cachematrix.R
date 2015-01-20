#### 
## cacheSolve.R
## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 

####
## Function makeCacheMatrix creates a special "matrix", which is really a
## list containing a function to:
##	1.	set the value of the matrix
##	2.	get the value of the matrix
##	3.	set the value of the inverse
##	4.	get the value of the inverse
##   This "matrix" can cache its inverse.
##  @param - matrix

makeCacheMatrix <- function(x = matrix(1,1,1)) {
        m <- NULL
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Get the value of the matrix        
        get <- function() x
        ## Cache inverse matrix
        setinverse <- function(inverse) m <<- inverse
        ## Get inverse matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
####
## The following function calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinverse function.
## @param - cacheable "matrix"

cacheSolve <- function(x, ...) {
        ## Check cache first
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Get matrix 
        data <- x$get()
        ## Calculate the inverse matrix
        m <- solve(data, ...)
        ## Cache the inverse matrix
        x$setinverse(m)
        m
}

