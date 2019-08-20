## the first function creates the platform for getting & setting the value or the matrix
## the second function helps to inverse the received value

## Getting & Setting the inputs

makeCacheMatrix <- function(x = matrix()) 
  {
    b <- NULL
    set <- function(y)
    {
      x <<- y
      b <<- NULL
    }
    get <- function () x
    setmatrixinverse <- function(inverse) b <<- inverse
    getmatrixinverse <- function() b
    list (set = set, get = get, setmatrixinverse = setmatrixinverse, getmatrixinverse = getmatrixinverse)
  }


## Inverse the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    b <- x$getmatrixinverse()
    if(!is.null(b)) {
      message("getting cached data...")
      return(b)
    }
    data <- x$get()
    b <- solve(data)
    x$setmatrixinverse(b)
    b
  }
