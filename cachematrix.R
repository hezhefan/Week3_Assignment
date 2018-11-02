## makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
        ## Create an empty inverse
        i <- NULL
        ## SetMatrix
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        
        ## GetMatrix
        get <- function() {
                m
        }
        
        ## SetInverse & give i a real value
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## GetInverse
        getInverse <- function(){
                i
        }
        
        ## the return value/list of this function
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}

##-----------------------------------------------------------

## cacheSolve: This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        ## get an inverse of a matrix-x, this inverse is also a matrix-m
        m <- x$getInverse()
        
        ## return m if the function above return a un-null value
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        ## calulation part using default function
        inverse <- solve(data)
        
        ## now, we transferred the calculated m to setInverse
        x$setInverse(inverse)
        
        ## finally, we return m
        inverse
}


## testing
## x <- makeCacheMatrix(matrix(5:8, 2, 2))
## cacheSolve(x)










