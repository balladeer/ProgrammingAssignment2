# The two functions below allow a user to speed up the
# calculation of a matrix's inverse, by caching the result
# of the solve() function.  This means that the actual
# call to solve() only has to be performed once, as long
# as the matrix in question is not changed.


# makeCacheMatrix - convert the matrix 'x' into a cache-able matrix
makeCacheMatrix <- function(x = matrix()) {
    cached_solution <- NULL

    # This function is used to 'set' the value of the original matrix
    set <- function(y){
        x <<- y
        cached_solution <<- NULL
    }

    # This function returns the original matrix
    get <- function() x

    # This function is used internally to store the results of solve()
    set_cache <- function(inverse_matrix) cached_solution <<- inverse_matrix

    # This function allows cacheSolve() to use the cached inverse matrix
    get_cache <- function() cached_solution

    # Here we return a list that contains the functions needed to operate
    # our 'special' cache-able matrix.
    list(set = set,
         get = get,
         set_cache = set_cache,
         get_cache = get_cache)
}


# Solve a matrix created via makeCacheMatrix.  Returns a cached solution if possible.
cacheSolve <- function(x, ...) {
    cached_solution <- x$get_cache()  # Get the currently-stored cache value

    if(!is.null(cached_solution)) {  # If the cache value is not null, return it
        message("getting cached solution")
        return(cached_solution)
    }

    # If we've gone this far, then the stored inverse matrix is NULL, so we
    # need to calculate the inverse matrix via the solve() function.
    data <- x$get()
    inverse_matrix <- solve(data)
    x$set_cache(inverse_matrix)

    inverse_matrix
}
