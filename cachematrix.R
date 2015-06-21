#This is the code for assignment 2
#There are two functions in this assignment:makeCacheMatrix and 
#cacheSolve 
#The basic idea of this a pair of functions is to cache the inverse
#Of a matrix.

#makeCacheMatrix creates a special "vector", which is really a list 
#containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        #I could also set m as follows
        #m <- matrix(NA,nrow(x),ncol(x))
        set <- function(y) {
        x <<- y
        m <- NULL
        #I could also set m as follows
        #m <- matrix(NA,nrow(x),ncol(x))
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#cachemean calculated the inverse matrix
#if a inverse has already been calculated,return it directly
#Otherwise,calculate the inverse matrix

cachemean <- function(x, ...) {
        m <- x$getinv()
        #if a inverse has already been calculated,return it directly
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
        #Otherwise,calculate the inverse matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        #return the inverse matrix
        m
}