## makeCacheMatrix takes in a matrix object and returns a list of stored functions created within it. The list can be called
## to set and retrieve data.
## cacheSolve takes in a matrix object, determines if the inverse has been calculated and if not it calculates the inverse
## and then returns it.

# The makeCacheMatrix function takes in a matrix 'x'. The object 'm' is set to NULL intially as a placeholder within the
# function. set is an internal function that assigns 'y' the matrix 'x' and sets the inverse variable 'm' to NULL. get is
# assigned a function that returns 'x'. setinv is assigned a function call to the R function solve, it returns the matrix
# inverse 'm'. getinv is assigned a function that returns 'm'. A list is created that can return all of the created
# information from the makeCacheMatrix function call.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                   
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# cacheSolve takes in a matrix 'x'. It checks the environment list to see if the matrix inverse 'm' has already been solved 
# for and if not then uses the R solve function to calculate it. Once the inverse 'm' is calculated it is then set and also
# returned. If the inverse has already been calculated the function returns this stored inverse.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
