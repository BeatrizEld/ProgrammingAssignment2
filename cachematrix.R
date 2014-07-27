## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(matrix) {
                M <<- matrix
                Inv <<- NULL
        }
        get <- function() {M}
        setInv <- function(inverse) {Inv <<- inverse}
        getInv <- function() {Inv}
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        Inv <- M$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- M$get()
        Inv <- solve(data) %*% data
        M$setInv(Inv)
        Inv
}
