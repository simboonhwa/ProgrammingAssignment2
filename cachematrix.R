###################################################################
##
## CLASS: 	makeCacheMatrix is an object class. An instance is 
##		created when it is initialised
##
## ARGUMENT:	Matrix object
##
## METHOD: 	set, get (to set or return the matrix or x) 
##		setinv, getinv (to set or return the INVERSE matrix or m) 
##
## ATTRIBUTE: 	x (to store the value of matrix)
##		m (to store value of the inverse matrix)
##
## eg. 	matrixobject <- makeCacheMatrix(mymatrix)
##      ^ instance	^ class
##
##	matrixobject$get()
##		     ^ method
##
## Lexical scope: only "set" & "setinv" functions need to use "<<-"
##	to refer to attribute x(matrix)& m(inv matrix) within 
##	the instance (matrixobject). Without "<<-", it will complain object not
##	found during runtime.
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
##
## ARGUMENT:	Matrix object
##
## Return: 	Inverse matrix
##
## eg. 	myinvmatrix <- cacheSolve(mymatrix)
##	^ inverse matrix	   ^ matrix
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

##################################################################
##
## Example: 	
##	B = matrix(c(2, 4, 3, 1,),nrow=2,ncol=2) 	
## 	** create mymatrix object with B (x=B, m=NULL) 	
## 	mymatrix <- makeCacheMatrix(B) 
##	** to find inv of B, execute cacheSolve (x=B, m=(inv)B)
##	myinvmatrix <- cacheSolve(mymatrix)
##	** if excute cacheSolve(mymatrix) again, it will return m which already
##	** store the (inv)B in m
##
## 	** create mymatrix object with empty matrix (x=NULL, m=NULL)	
## 	mymatrix <- makeCacheMatrix() (x=NULL, m=NULL)
## 	** To assign mymatrix object with B matrix (x=B, m=NULL)	
## 	mymatrix.set(B) 
##################################################################

