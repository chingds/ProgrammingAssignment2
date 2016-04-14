## This little script takes a matrix input from the console, 
## and returns the inverse

##makeCacheMatrix takes the matrix input from the console, 
##set the value of the matrix with the set function
##get the value of the matrix with the get function
##set the value of the inverse matrix with the setinverse function
##get the value of the inverse matrix with the getinverse function

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##cacheSolve takes the matrix defined by the makeCacheMatrix function using get
##returns the inverse of the matrix with solve and return it as m

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m        ## Return a matrix that is the inverse of 'x'
}
