## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize a variable to store the cached inverse
        cached_inverse <- NULL
        
        ## Define a function to set the matrix
        set <- function(matrix) {
                x <<- matrix
                ## Invalidate the cached inverse when the matrix is changed
                cached_inverse <<- NULL
        }
        
        ## Define a function to get the matrix
        get <- function() {
                x
        }
        
        ## Define a function to cache the inverse of the matrix
        cache_inverse <- function(inverse) {
                cached_inverse <<- inverse
        }
        
        ## Define a function to get the cached inverse
        get_cached_inverse <- function() {
                cached_inverse
        }
        
        ## Return a list of functions
        list(set = set, 
             get = get, 
             cache_inverse = cache_inverse, 
             get_cached_inverse = get_cached_inverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Retrieve the cached inverse if available
        cached_inverse <- x$get_cached_inverse()
        if (!is.null(cached_inverse)) {
                message("getting cached inverse")
                return(cached_inverse)
        }
        
        ## If the cached inverse is not available, compute the inverse
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        
        ## Cache the inverse for future use
        x$cache_inverse(inverse)
        
        ## Return the computed inverse
        inverse
}
