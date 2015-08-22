## Following function demonstrate use of Lexical scoping to create caching functionality for inverse matrix function


## Function requires non-singular matrix as input, to generate list of functions which help set/get and Lexical scoped variables used for caching

makeCacheMatrix <- function(mx = matrix()) {
        inv <- NULL 
        set <- function(y) {
                mx <<-y   
                inv <<- NULL 
        }
        get <- function() mx
        setinv <- function(invm) inv <<- invm
        getinv <- function() inv
        list(set=set, get=get,setinv=setinv,getinv=getinv)

}


## Funtions takes cache matrix, which is list of functions generate by above and returns inverse of matrix
## If cache value of inverse is available it is returned, else it is calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
