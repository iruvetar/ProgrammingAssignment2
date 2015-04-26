## In this assignment, two functions are created to simplify the time-consuming computation of inverse matrix

## The function of first function is:
## to preserve a matrix and its reverse matrix with four functions to get or set the value 

makeCacheMatrix <- function(x = matrix()) {
  
    i <- NULL
  
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x

    setinverse <- function(inverseMatrix) i <<- inverseMatrix
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function is to return an inverse matrix of the matrix we preserved in the first function
## if the reverse matrix is already set, it will be returned directly
## if the reverse matrix is empty in the first function, it will be calculated by "solve" here

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached matrix")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    
    i
  
}
