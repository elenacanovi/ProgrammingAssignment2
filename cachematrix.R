## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix and cacheSolve work in exactly the same way as the makeVector and cachemean functions in https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping, the only differences being that here instead of vectors we have matrices and instead of taking the mean we campute the inverse matrix. 

## Write a short comment describing this function
# makeCacheMatrix has as 
# input:  a (invertible) matrix x 
# output: a list with four functions: the getter and setter of the input matrix and the getter and setter of the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        set <- function(y) {
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_x_value) inverse_x <<- inverse_x_value 
        getinverse <- function() inverse_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#cacheSolve has as
# input:  a cachematrix as obtained with the function makeCacheMatrix above
# output: if the inverse of the matrix is already saved in the input cachematrix, it is just returned (return(inverse_x)). If the inverse matrix is not already in the cache, it is computed in the line:  
# inverse_x <- solve(data, ...)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- x$getinverse()
        if(!is.null(inverse_x)) {
                message("getting cached data")
                return(inverse_x)
        }
        data <- x$get()
        inverse_x <- solve(data, ...)
        x$setinverse(inverse_x)
        inverse_x
}
