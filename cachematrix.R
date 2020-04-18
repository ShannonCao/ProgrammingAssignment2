## The first function makeCacheMatrix is used to compute and cache the inverse of a matrix. The second function 
## cacheSolve is used to compute the inverse of the matrix returned by the frist function. If the inverse has 
## been calculated by makeCacheMatrix for the same original matrix, then cacheSolve will return the inverse 
## computed from makeCacheMatrix. 


## makeCacheMatrix has 4 functions: set(), get(), setinverse(), getinverse(), and 2 data objects: m and x. The overall 
## goal of makeCacheMatrix is to create a matrix, then compute and store the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL     
        }
        get <- function()x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The advantage of lexical scopping of R enables cacheSolve to access the values of x and m through the 
## the use of 4 functions defined in makeCacheMatrix. Also, by using the name of defined functions, we can 
## access the funcstions with $. The overall goal of cacheSolve is to test if the inverse of the original 
## matrix has been calculated by makeCacheMatrix, if it does, cacheSolve would return the inverse. If not, 
## cacheSolve would compute the inverse, then set and return the inverse. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        mydata <- x$get()
        m <- solve(mydata, ...)
        x$setinverse(m)
        return(m)
}
