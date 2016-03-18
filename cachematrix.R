## Calculating the Inverse of a matrix could be a intensive computation
## The below functions will take advantage of the scoping rules of 
## the R language and manipulate them to preserve state inside of an R object.

## creates a special matrix which contains functions to 
## get and set the matrix as well as the inverse

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y){
              x <<- y
              inv <<- NULL
          }
          
          get = function() x
          setinverse <- function(inverse) inv <<- inverse
          getinverse <- function() inv
          list(set = set, get = get,
               setinverse = setinverse, getinverse = getinverse)
}


## compute the inverse of the matrix if the inverse value is not 
## already in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
              message("getting cached inverse")
              return (inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
