# The first function, "makeCacheMatrix" creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the inverse value of the matrix
# get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

# The following function calculates the inverse of the special "matrix" created with the "makeCacheMatrix" function.
# It is assumed that the matrix is invertible - so there is no checking on that part.
# Function first checks to see if the inverse of this matrix is already calculated and available in the cache.
# If available in the cache, the function gets the inverse from the cache and skips the computation.
# Otherwise, the function calculates the inverse using the R function solve() and sets the inverse of the 
# matrix in the cache via the "setmatrix" function.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
