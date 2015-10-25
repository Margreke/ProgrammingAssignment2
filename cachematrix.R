##  Name:    cachematric.R
##  Purpose: these 2 functions cache the inverse of a matrix. 

    # -- Try out -- 
    # a <- matrix(1:4, nrow = 2, ncol = 2)
    # solve(a)
    # a1 <- makeCacheMatrix(a)
    # cacheSolve(a1)
    #
    # -- --- --- --

##  Function 1
##
##  This function creates a special "matrix" object,
##  which is really a list containing a function to set the matrix, 
##  get the matrix, set the inverse matrix values, get the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
  
                m <- NULL
                set <- function(y) {  
                  x <<- y
                  m <<- NULL
                }
                
                get <- function() x      
                setsolve <- function(solve) m <<- solve  
                getsolve <- function() m   
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
                
}

##  Function 2
##
##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##  has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
          
          m <- x$getsolve()
          if(!is.null(m)) {
            message("getting cached data")
            return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setsolve(m)
          m
  
        ## Return a matrix that is the inverse of 'x'
}

