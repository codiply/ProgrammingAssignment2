## A pair of functions that cache the inverse of a matrix.
##
## Example:
##   n <- 1000
##   mat <- matrix(data = rnorm(n*n), nrow = n, ncol = n)
##   cachedMat <- makeCacheMatrix(mat)
##   inv <- cacheSolve(cachedMat)
##   inv <- cacheSolve(cachedMat) ## Call it a second time
##   sum((inv %*% mat-diag(n))^2) ## This should be zero

#' Creates a container object for a matrix and its inverse.
#'
#' The set and get methods store and retrieve the underlying matrix.
#' The setsolve and getsolve methods store and retrieve its inverse matrix.
#' @param x The underlying matrix.
#' @return A list of functions for setting and getting the matrix and its inverse.
#' #' @examples
#' mat <- matrix(data = rnorm(100), nrow = 10, ncol = 10); makeCacheMatrix(mat)
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(sol) s <<- sol
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


#' Caches and returns the inverse of a matrix.
#'
#' @param x A wrapped matrix using the function makeCacheMatrix.
#' @return The inverse matrix.
#' #' @examples
#' cachedMatrix <- makeCacheMatrix(mat); inv <- cacheSolve(cachedMatrix)
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
            message("getting cached data")
            return(s)
        }
        mat <- x$get()
        s <- solve(mat, ...)
        x$setsolve(s)
        s
}