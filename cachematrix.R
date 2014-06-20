# These functions provide for computing and caching the inverse
# of a matrix. Since computation of an inverse is often expensive,
# the computed inverse is stored in a cache and is returned provided the 
# original matrix has not changed.


# This function provides for a set of sub-function that can set, get 
# compute the inverse of a matrix. This function returns a list of
# functions that can then be used with the cacheSolve function (below)
# to access the matrix and its inverse
makeCacheMatrix <- function(mat = matrix()) {
    # Initialize the variable mat_inv to null such that the functions
    # defined below can access them (i.e. this is the parent frame to
    # the functions defined below)
    mat_inv <- NULL

    # Given an input matrix, set it. Also "clear the cache" 
    # (mat_inv <<- NULL) since the original matrix has likely changed
    set <- function(input_mat)
    {
        mat <<- input_mat
        mat_inv <<- NULL
    }

    # The get function simply returns the matrix
    get <- function() mat

    # The setinv function assigns the provided matrix inverse to 
    # the local mat_inv (this also caches the matrix inverse)
    setinv <- function(minv) mat_inv <<- minv

    # The getinv function simply returns the (cached) matrix inverse
    getinv <- function() mat_inv

    # Return a function list of the cache matrix manipulation functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function is the access function to the cached matrix inverse
# When provided with the function list from the makeCachematrix function,
# it will attempt to access the cached inverse. If the cached inverse is
# not available, it will compute the inverse, cache it and return the inverse
cacheSolve <- function(func_list, ...) {
    # Get the cached inverse
    mat_inv <- func_list$getinv()
    # If inverse has been cached from before, return the cached version
    if(!is.null(mat_inv))
    {
        message("Returning cached matrix inverse")
        return(mat_inv)
    }

    # If not, get the matrix, compute inverse, cache it and return the inverse
    mat <- func_list$get()
    mat_inv <- solve(mat, ...)
    func_list$setinv(mat_inv)
    mat_inv
}
