## Data Science - Course 2 Assignment

# Create Function 1
# makeCacheMatrix: This function creates a special "matrix" object
#   that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    # Note: <<- operator - assign value to an object in an env diff from
    #   current env
    m <- NULL
    # 1) set value of matrix object x to be y
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    # get value of matrix object x
    get <- function() x
    # compute inverse of matrix object m (to be cached)
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Create Function 2
# cacheSolve: This function computes the inverse of the special 
#   "matrix" returned by makeCacheMatrix above. If the inverse has
#   already been calculated (and the matrix has not changed), then 
#   the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    # Return a matrix that is the inverse of 'x'
    # Using solve(x) function
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

# cacheSolve(makeCacheMatrix(<matrix_object>))
