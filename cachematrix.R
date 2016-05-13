# Functions to cache the inverse of a matrix
# Training results:
# 
# vb_matrix <- makeCacheMatrix(matrix(c(8, 4, 4, 8, 0, 0, 2, 2, 6), c(3, 3)))
# cacheSolve(vb_matrix)
# 
# If there is no inverse cached we will get the following:
# >       [,1]    [,2]    [,3]
# > [1,] 0.000  0.3750 -0.1250
# > [2,] 0.125 -0.3125  0.0625
# > [3,] 0.000 -0.2500  0.2500
#
# If there is inverse already cached we will get the following:
# > getting cached data
# >       [,1]    [,2]    [,3]
# > [1,] 0.000  0.3750 -0.1250
# > [2,] 0.125 -0.3125  0.0625
# > [3,] 0.000 -0.2500  0.2500

# Create a special "matrix", which is a list containing
# a function to
#   - set the value of the matrix
#   - get the value of the matrix
#   - set the value of the inverse matrix
#   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


# Calculate the inverse of the special "matrix" created with the above
# function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
