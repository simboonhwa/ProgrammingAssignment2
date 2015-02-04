###################################################################
##
## CLASS: 	makeCacheMatrix is an object class. An instance is 
		created when it is initialised
## METHOD: 	set, get (to set or return the matrix) 
##		setinv, getinv (to set or return the inverse matrix) 
## ATTRIBUTE: 	m (to store the inverse matrix)
##
## eg. 	matrixobject <- makeCacheMatrix(mymatrix)
##      ^ instance	^ class
##
##	matrixobject$get()
##		     ^ method
###################################################################

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL;
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        getinv <- function() m
        setinv <- function(inv) m <<- inv
        get <- function() x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

###################################################################
##
## FUNCTION:	cachsolve is a function that will evaluate 
##		makeCacheMatrix instance and either compute and
##		store or return the inverse matrix
## Return: 	Inverse matrix
##
###################################################################

cacheSolve <- function(x, ...) {
	        m <- x$getinv()
        
	if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
