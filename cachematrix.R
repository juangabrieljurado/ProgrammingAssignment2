
# This function, makeCacheMatrix, makes a cache matrix from a given x.
makeCacheMatrix <- function(x = matrix()) {
        
        # I set the value of the matrix.
        CacheMatrix <-NULL
        setMatrix <- function(y){
                x <<- y
                CacheMatrix <<- NULL
        }
        
        #Then, I return the matrix.
        getMatrix <- function () x
        
        #After that, I set the cache thought a 'setCache' function.
        setCache <- function(inverse) CacheMatrix <<- inverse
        
        #Finally, I get the cache inversed in the following function.
        getCache <- function () CacheMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setCache = setCache,
             getCache = getCache)
}


#This function 'cacheSolve' allows me to get a matrix with the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        #First, I define 'cacheMatrix' and get the cache.
        cacheMatrix <- x$getCache()
        
        #Then, it checks if the cachematrix is calculated. If so, it gets the cachematrix.
        if (!is.null(cacheMatrix)){
                message("loading cache matrix...")
                return(CacheMatrix)
        }
        #Otherwise, it calculates the value thought setCache function.
        else {
                data <- x$getMatrix()
                CacheMatrix <- solve(data,...)
                x$setCache(CacheMatrix)
                return(CacheMatrix)
        }
}
