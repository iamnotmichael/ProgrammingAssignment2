## Functions solve for the inverse of a matrix and cached the results to speed up calculations

## Function retrieves matrix and caches inverse matrix calcuations
makeCacheMatrix <- function(x = matrix()) {
        # Function is initialized with minv variable and set function is set up to 
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        
        # Getter for matrix 'x'
        get <- function() x
        
        # Setter for caching inverse matrix
        setmatinv <- function(solve) minv <<- solve
        
        # Getter for cached inverse cached matrix
        getmatinv <- function() minv
        
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}

## Function solves for the inverse of a matrix. Function initially looks if inverse has been calculated previously and returns 
## cached value if TRUE. Otherwise, function calculates inverse of matrix, caches result, and then returns inverse of matrix
cacheSolve <- function(x, ...) {
        ## Function creates variable with results from get function in makeCacheMaterix. If value is not null, then the cached 
        ## inverse matrix has been calculated and the value is returned
        minv <- x$getmatinv()
        if(!is.null(minv)) {
                message("Inverse Matrix calculated, retrieving cached inverse calcuation")
                return(minv)
        }
        
        ## Function assigns variable by getting the matrix inputted in makeCacheMatrix and then uses the solve function to 
        ## find inverse matrix.
        mCMget <- x$get()
        minv <- solve(mCMget)
        
        ## Function sets inverse matrix in makeCacheMatrix so it can be cached in the future and inverse matrix is returned
        x$setmatinv(minv)
        minv
}
