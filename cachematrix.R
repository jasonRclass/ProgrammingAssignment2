@@ -4,6 +4,21 @@
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(i) m <<- i
    getInverse <- function() m
    
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)

}

@@ -12,4 +27,17 @@ makeCacheMatrix <- function(x = matrix()) {

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setInverse(m)
    
    m
}
