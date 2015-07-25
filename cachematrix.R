## These functions are used to create an object which will store a matrix
## and cache its inverse.

## This first function creates the special matrix. 

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set_matr <- function(y) {
                x <<- y
                m <<- NULL
        }
        get_matr <- function() x
        set_solve <- function(solve) m <<- solve
        get_solve <- function() m
        list(get_matr = get_matr, set_matr = set_matr,  
             get_solve = get_solve, set_solve = set_solve)
}


## This function computes the inverse of the special matrix created above.
## It will first check to see if inverse has already been calculated, and 
## if so, will retrieve from cache.

cacheSolve <- function(x, ...) {
        
        m <- x$get_solve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get_matr()
        m <- solve(data, ...)
        x$set_matr(m)
        m
        ## Return a matrix that is the inverse of 'x'
  }
