## Put comments here that give an overall description of what your
#
#   These functions need a square non-singular (invertible) input matrix
#   The following was used for testing:
#       set.seed(10)
#       x <- matrix(rnorm(100),10,10)
#       v <- makeCacheMatrix(x)
#       v_inv <- cacheSolve(v)
#
#       The first time cacheSolve() is run after makeCacheMatrix() the message
#       "Generating cached matrix" will appear, it will not appear on subsequent
#       calls to cacheSolve() until makeCacheMatrix() is called again
#
#
## Write a short comment describing this function
#   Input:  square data matrix
#   Output: a structure which contains:
#               original matrix
#               inverse matrix
#               some calling functions for storing and getting either of the two matrices
#                       set, get - save, return original matrix
#                       setinv, getinv - save, return the inverse matrix
#
#
makeCacheMatrix <- function(x = matrix()) {
      x_inv <- NULL                                # initialize pointer to inverted matrix
                                                   # NULL indicates the inverse needs to be calculated
      set <- function(y) {                         
        x <<- y                                    # store the original matrix
        x_inv <<- NULL
      }
      get <- function() x                          # get the original matrix
      setinv <- function(solve) x_inv <<- solve    # save the inverted matrix
      getinv <- function() x_inv                   # get the cached inverted matrix
      list(set = set,
           get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
#
#   This is a caching version if R's solve() function
#   Called whenever the inverse matrix of the original matrix stored using makeCacheMatrix() is needed
#   The inverted matrix is calculated the first time it's called and result is cached
#   The cached version of the inverted matrix is then used on subsequent calls
#
#   Input:  the structure returned by makeCacheMatrix
#   Output: the inverse of the original matrix
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      x_inv <- x$getinv()                            # has the inverse matrix been computed?
      if(is.null(x_inv)) {                           # non NULL indicates inverse has already been calculated
            message("Generating cached matrix")
            x_orig <- x$get()                        # if not already calculated
            x_inv <- solve(x_orig, ...)              # compute the inverse
            x$setinv(x_inv)                          # cache it
      }
      x_inv                                          # return inverse matrix
}
