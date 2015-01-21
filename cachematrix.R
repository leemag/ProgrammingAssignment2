## makeCacheMatrix and cacheSolve are functions which operate
## on a matrix, storing the inverse matrix in the environment
## so that it may be recovered rather than calculating the
## inverse when it is needed.  This is expected to dramatically
## decrease inverse matrix calculation time for large matrices

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        
}


## Compute the inverse of the special "matrix" object
## returned by the makeCacheMatrix function. If the object does not
## exist when this function is first called, calculate the 
## inverse matrix and store it in the envronment.  
## If it is already there, retrieve it and check that
## it has not changed since it was originally cached.

cacheSolve <- function(x, ...) {
        ## Get a matrix that is the inverse of 'x'
        m <- x$getinv()
        ## Check if inverse matrix has already been cached return it.
        if(!is.null(m)) {
                message("getting cached data")
                # test that cached value hasn't changed by multiplying with
                # original and comparing product to the identity matrix
                calc <- round(m %*% x$get())
                unit <- diag(, nrow = nrow(m), ncol = ncol(m))
                if(!identical(calc, unit)){
                       message("bad cache data")
                       exit()
               }
                return(m)
        }
        ## if it wasnt't already cached, calculate the inverse matrix, store it, and
        ## return it.
        message("creating new cached inverse")
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
