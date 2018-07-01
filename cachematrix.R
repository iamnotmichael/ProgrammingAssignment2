## Functions solve for the inverse of a matrix. Initially, the function checks to see if the inverse has already been
## calculated and retuns that value to speed up calcuations. If not, the inverse is calculated and cached.

## Function retrieves and sets inverse matrix calcuations for caching

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setmatinv <- function(solve) minv <<- solve
        getmatinv <- function() minv
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}

## Function solves for the inverse of a matrix. Function initially looks if inverse has been calculated previously 
## and returns cached value if TRUE
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getmatinv()
        if(!is.null(minv)) {
                message("inverse calculated, retrieving cached inverse calcuation")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setmatinv(minv)
        minv
}
