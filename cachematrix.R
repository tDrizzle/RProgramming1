## Creates, stores and calls a matrix or its inverse from  
## the chosen cache

## makeCacheMatrix creates custom matrix and runs 4 functions:
## 1. "set" sets value of matrix
## 2. "get" recalls value of matrix
## 3. "setmatrix" stores inverse of matrix in cache 
## 4. "getInverse" recalls matrix inverse in cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set<-function(y){
                x <<- y
                m <<- NULL
        }
        get<-function() x
        setInverse <- function(solve) m<<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve returns a matrix that is X's inverse

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setInverse(m)
        m
}

        ## Return a matrix that is the inverse of 'x'
}
