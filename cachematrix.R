## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         dm <- dim(x)
         im <- matrix(nrow=dm[1],ncol=dm[2])
         set <- function(y = matrix()) {
                 x <<- y
                 dm <- dim(x)
                 im <<- matrix(nrow=dm[1],ncol=dm[2])
         }
         get <- function() x
         setinv <- function(inv) im <<- inv
         getinv <- function() im
         list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                      im <- x$getinv()
              if(all(!is.na(im))) {
                          message("getting cached inverse")
                          return(im)
                      }
              data <- x$get()
              message("calculating inverse")
              im <- solve(data, ...)
              x$setinv(im)
              im
}