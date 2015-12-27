## These two functions cache the inverse of a matrix so that the user can look up the results easily instead of recomputing the value of the inverse. 

## The makeCacheMatrix creates a special list that's really a function to:
## 1.set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function will return a matrix that is the inverse of the matrix "x". It first checks to see if the inverse of the matrix, "inv", has been set. If it has, then it returns the inverse. If it hasn't, it then computes the inverse  ("solve(data,...)") and sets the value using the "setinverse" function we created earlier.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

