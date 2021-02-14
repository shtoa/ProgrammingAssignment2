# This is a function that creates a special matrix that is able to cache itself

makeCacheMatrix <- function(x = matrix()) {
        # Currently the solution is set to NULL
        s <- NULL
        # This function sets the value of the matrix based on the input to the function
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        # This allows to return the cached matrix
        get <- function(){x}
        # This caches the inverse of the matrix 
        setsolve <- function(inv){
                s <<- inv
        }
        # This allows to return the cached solution (inverse) of the matrix
        getsolve <- function(){s}
        
        # This is a list of functions that a cache matrix can do
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


# This function checks if a solution to a matrix is cached and if not creates it

cachesolve <- function(x, ...) {
        # Checks if the solution is cached
        s <- x$getsolve()
        if(!is.null(s)) {
                # prints message that the solution already exists
                message("getting cached data")
                return(s)
        }
        #If solution doesnt exist the matrix is solved and cached 
        matrix <- x$get()
        s <- solve(matrix, ...)
        x$setsolve(s)
        # the solution is then returned
        s
}
