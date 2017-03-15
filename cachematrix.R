## Creates the caching mechanism for solving the inverse of a matrix

## This function sets up the data structures necessary for caching, and add that functionality to whatever matrix is passed to it.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL;
    
    set <- function(y){
        x <<- y;
        m <<- NULL;
    }
    
    get <- function() x;
    
    setInverse <- function(solve) m <<- solve;
    
    getInverse <- function() m;
    
    list(set=set, get=get, setInverse = setInverse, getInverse = getInverse);
    
}


## This function will first check the cache to see if a matrix is solved, and if it isn't then it will solve it and store it to the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse();
    if(!is.null(m)){
        message("Getting Cached Data");
        return(m);
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
