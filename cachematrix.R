## The following pair of functions creates a cache of the inverse of a matrix.
## When there is no inverse of a matrix in the cache, a new one will be 
## computed. When a inverse of a matrix is present, this will be retrieved.

## makeCacheMatrix() creates a list containing the following:
## 1. set the value of the matrix;
## 2. get the value of the matrix;
## 3. set the value of inverse of the matrix;
## 4. get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve() returns the inverse of a matrix. First it checks whether the
## inverse of the matrix is already cached. If yes, that is shown. If no, the
## inverse is computed by the function and subsequently cached.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}
