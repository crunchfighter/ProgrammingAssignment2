## This takes a matrix M, caches a list of functions to store its 
## inverse.  
##
## First run y <- makeCacheMatrix(M)
## Then run  z <- cacheSolve(y) 
## If run the first time, it will calculate and return the 
## inverse to z.
##
## If run subsequent times, it will return the inverse of z 
## from the list formed in makeCacheMatrix and avoid recomputing 
## the inverse


makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL                   ## initialize the invM
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setInv <- function(solve) invM <<- solve
        getInv <- function() invM
        list(set = set, get = get,      
             setInv = setInv,
             getInv = getInv)          ## set the list of functions
}


## After the y <- makeCacheMatrix(M) function is run:
## z <- cacheSolve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInv()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data)
        x$setInv(invM)
        invM
}
