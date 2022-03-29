

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                           
    set <- function(y) {                   
        x <<- y                            
        inv <<- NULL                       
    }
    get <- function() x                    

    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                                                                  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.


cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
  
}
