#This is the code for assignment 2
#There are two functions in this assignment:makeCacheMatrix and 
#cacheinverse 
#The basic idea of this a pair of functions is to cache the inverse
#Of a matrix.

#makeCacheMatrix creates a special "vector", which is really a list 
#containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
         }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

#cacheinverse calculated the inverse matrix
#if a inverse has already been calculated,return it directly
#Otherwise,calculate the inverse matrix

cacheinverse <- function(x, ...) {
        inv <- x$getinv()
        #if a inverse has already been calculated,return it directly
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        #Otherwise,calculate the inverse matrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        #return the inverse matrix
        inv
}
